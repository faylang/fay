{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Compile pattern matches.

module Fay.Compiler.Pattern where

import           Fay.Compiler.Misc
import           Fay.Compiler.QName
import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.NoAnnotation           as N
import qualified Fay.Exts.Scoped                 as S
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names

-- | Compile the given pattern against the given expression.
compilePat :: JsExp -> S.Pat -> [JsStmt] -> Compile [JsStmt]
compilePat exp pat body = case pat of
  PVar _ name       -> compilePVar name exp body
  PApp _ cons pats  -> do
    newty <- lookupNewtypeConst cons
    case newty of
      Nothing -> compilePApp cons pats exp body
      Just _  -> compileNewtypePat pats exp body
  PLit _ literal    -> compilePLit exp literal body
  PParen{}          -> shouldBeDesugared pat
  PWildCard _       -> return body
  pat@PInfixApp{}   -> compileInfixPat exp pat body
  PList _ pats      -> compilePList pats body exp
  PTuple _ _bx pats -> compilePList pats body exp
  PAsPat _ name pat -> compilePAsPat exp name pat body
  PRec _ name pats  -> compilePatFields exp name pats body
  pat               -> throwError (UnsupportedPattern pat)

-- | Compile a pattern variable e.g. x.
compilePVar :: S.Name -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePVar (unAnn -> name) exp body =
  return $ JsVar (JsNameVar (UnQual () name)) exp : body

-- | Compile a record field pattern.
compilePatFields :: JsExp -> S.QName -> [S.PatField] -> [JsStmt] -> Compile [JsStmt]
compilePatFields exp name pats body = do
  c <- liftM (++ body) (compilePats' [] pats)
  qname <- unsafeResolveName name
  return [JsIf (force exp `JsInstanceOf` JsConstructor qname) c []]
  where
    -- compilePats' collects field names that had already been matched so that
      -- wildcard generates code for the rest of the fields.
      compilePats' :: [S.QName] -> [S.PatField] -> Compile [JsStmt]
      compilePats' _ (p@PFieldPun{}:_) = shouldBeDesugared p
      compilePats' names (PFieldPat _ fieldname (PVar _ (unAnn -> varName)):xs) = do
        r <- compilePats' (fieldname : names) xs
        return $ JsVar (JsNameVar (UnQual () varName))
                       (JsGetProp (force exp) (JsNameVar (unQualify $ unAnn fieldname)))
                 : r -- TODO: think about this force call

      compilePats' names (PFieldWildcard (wildcardFields -> fields):xs) = do
        f <- forM fields $ \fieldName ->
          return $ JsVar (JsNameVar fieldName)
                         (JsGetProp (force exp) (JsNameVar fieldName))
        r <- compilePats' names xs
        return $ f ++ r

      compilePats' _ [] = return []

      compilePats' _ (pat:_) = throwError (UnsupportedFieldPattern pat)

      wildcardFields :: S.X -> [N.QName]
      wildcardFields l = case l of
        Scoped (RecPatWildcard es) _ -> map (unQualify . origName2QName) es
        _ -> []

-- | Compile a literal value from a pattern match.
compilePLit :: JsExp -> S.Literal -> [JsStmt] -> Compile [JsStmt]
compilePLit exp literal body = do
  c <- ask
  lit <- readerCompileLit c literal
  return [JsIf (equalExps exp lit)
               body
               []]

  where
    -- Equality test for two expressions, with some optimizations.
    equalExps :: JsExp -> JsExp -> JsExp
    equalExps a b
      | isConstant a && isConstant b = JsEq a b
      | isConstant a = JsEq a (force b)
      | isConstant b = JsEq (force a) b
      | otherwise =
         JsApp (JsName (JsBuiltIn "equal")) [a,b]

-- | Compile as binding in pattern match
compilePAsPat :: JsExp -> S.Name -> S.Pat -> [JsStmt] -> Compile [JsStmt]
compilePAsPat exp (unAnn -> name) pat body = do
  p <- compilePat exp pat body
  return $ JsVar (JsNameVar $ UnQual () name) exp : p

-- | Compile a pattern match on a newtype.
compileNewtypePat :: [S.Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compileNewtypePat [pat] exp body = compilePat exp pat body
compileNewtypePat ps _ _ = error $ "compileNewtypePat: Should be impossible (this is a bug). Got: " ++ show ps

-- | Compile a pattern application.
compilePApp :: S.QName -> [S.Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePApp cons pats exp body = do
  let forcedExp = force exp
  let boolIf b = return [JsIf (JsEq forcedExp (JsLit (JsBool b))) body []]
  case cons of
    -- Special-casing on the booleans.
    Special _ (UnitCon _) -> return (JsExpStmt forcedExp : body)
    UnQual _ (Ident _ "True")   -> boolIf True
    UnQual _ (Ident _ "False")  -> boolIf False
    -- Everything else, generic:
    n -> do
      let n' = tryResolveName n
      case n' of
        Nothing -> error $ "Constructor '" ++ prettyPrint n ++ "' could not be resolved"
        Just _ -> do
          recordFields <- map (UnQual ()) <$> recToFields n
          substmts <- foldM (\body (field,pat) ->
                                 compilePat (JsGetProp forcedExp (JsNameVar field)) pat body)
                      body
                      (reverse (zip recordFields pats))
          qcons <- unsafeResolveName cons
          return [JsIf (forcedExp `JsInstanceOf` JsConstructor qcons)
                       substmts
                       []]

-- | Compile a pattern list.
compilePList :: [S.Pat] -> [JsStmt] -> JsExp -> Compile [JsStmt]
compilePList [] body exp =
  return [JsIf (JsEq (force exp) JsNull) body []]
compilePList pats body exp = do
  let forcedExp = force exp
  stmts <- foldM (\body (i,pat) -> compilePat (JsApp (JsName (JsBuiltIn "index"))
                                                     [JsLit (JsInt i),forcedExp])
                                              pat
                                              body)
        body
        (reverse (zip [0..] pats))
  let patsLen = JsLit (JsInt (length pats))
  return [JsIf (JsApp (JsName (JsBuiltIn "listLen")) [forcedExp,patsLen])
               stmts
               []]

-- | Compile an infix pattern (e.g. cons and tuples.)
compileInfixPat :: JsExp -> S.Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp _ left (Special _ cons) right) body = case cons of
  Cons _ ->
    withScopedTmpJsName $ \tmpName -> do
      let forcedExp = JsName tmpName
          x = JsGetProp forcedExp (JsNameVar "car")
          xs = JsGetProp forcedExp (JsNameVar "cdr")
      rightMatch <- compilePat xs right body
      leftMatch <- compilePat x left rightMatch
      return [JsVar tmpName (force exp)
             ,JsIf (JsInstanceOf forcedExp (JsBuiltIn "Cons"))
                   leftMatch
                   []]
  _ -> throwError (UnsupportedPattern pat)
compileInfixPat _ pat _ = throwError (UnsupportedPattern pat)
