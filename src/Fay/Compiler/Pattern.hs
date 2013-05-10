{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile pattern matches.

module Fay.Compiler.Pattern where

import Fay.Compiler.Misc
import Fay.Types

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Language.Haskell.Exts

-- | Compile the given pattern against the given expression.
compilePat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePat exp pat body =
  case pat of
    PVar name       -> compilePVar name exp body
    PApp cons pats  -> do
      newty <- lookupNewtypeConst cons
      case newty of
        Nothing -> compilePApp cons pats exp body
        Just _  -> compileNewtypePat pats exp body
    PLit literal    -> compilePLit exp literal body
    PParen pat      -> compilePat exp pat body
    PWildCard       -> return body
    pat@PInfixApp{} -> compileInfixPat exp pat body
    PList pats      -> compilePList pats body exp
    PTuple pats     -> compilePList pats body exp
    PAsPat name pat -> compilePAsPat exp name pat body
    PRec name pats  -> compilePatFields exp name pats body
    pat             -> throwError (UnsupportedPattern pat)

-- | Compile a pattern variable e.g. x.
compilePVar :: Name -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePVar name exp body = do
  bindVar name
  return $ JsVar (JsNameVar (UnQual name)) exp : body

-- | Compile a record field pattern.
compilePatFields :: JsExp -> QName -> [PatField] -> [JsStmt] -> Compile [JsStmt]
compilePatFields exp name pats body = do
    c <- liftM (++ body) (compilePats' [] pats)
    qname <- unsafeResolveName name
    return [JsIf (force exp `JsInstanceOf` JsConstructor qname) c []]
  where -- compilePats' collects field names that had already been matched so that
        -- wildcard generates code for the rest of the fields.
        compilePats' :: [QName] -> [PatField] -> Compile [JsStmt]
        compilePats' names (PFieldPun name:xs) =
          compilePats' names (PFieldPat (UnQual name) (PVar name):xs)

        compilePats' names (PFieldPat fieldname (PVar varName):xs) = do
          r <- compilePats' (fieldname : names) xs
          bindVar varName
          return $ JsVar (JsNameVar (UnQual varName))
                         (JsGetProp (force exp) (JsNameVar fieldname))
                   : r -- TODO: think about this force call

        compilePats' names (PFieldWildcard:xs) = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
              fields' = fields \\ names
          f <- mapM (\fieldName -> do bindVar (unQual fieldName)
                                      return (JsVar (JsNameVar fieldName)
                                             (JsGetProp (force exp) (JsNameVar fieldName))))
                   fields'
          r <- compilePats' names xs
          return $ f ++ r

        compilePats' _ [] = return []

        compilePats' _ (pat:_) = throwError (UnsupportedFieldPattern pat)

        unQual (Qual _ n) = n
        unQual (UnQual n) = n
        unQual Special{} = error "Trying to unqualify a Special..."

-- | Compile a literal value from a pattern match.
compilePLit :: JsExp -> Literal -> [JsStmt] -> Compile [JsStmt]
compilePLit exp literal body = do
  c <- ask
  lit <- readerCompileLit c literal
  return [JsIf (equalExps exp lit)
               body
               []]

  where -- Equality test for two expressions, with some optimizations.
        equalExps :: JsExp -> JsExp -> JsExp
        equalExps a b
          | isConstant a && isConstant b = JsEq a b
          | isConstant a = JsEq a (force b)
          | isConstant b = JsEq (force a) b
          | otherwise =
             JsApp (JsName (JsBuiltIn "equal")) [a,b]

-- | Compile as binding in pattern match
compilePAsPat :: JsExp -> Name -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePAsPat exp name pat body = do
  bindVar name
  x <- compilePat exp pat body
  return ([JsVar (JsNameVar (UnQual name)) exp] ++ x)

compileNewtypePat :: [Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compileNewtypePat [pat] exp body = compilePat exp pat body
compileNewtypePat ps _ _ = error $ "compileNewtypePat: Should be impossible (this is a bug). Got: " ++ show ps

-- | Compile a pattern application.
compilePApp :: QName -> [Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePApp cons pats exp body = do
  let forcedExp = force exp
  let boolIf b = return [JsIf (JsEq forcedExp (JsLit (JsBool b))) body []]
  case cons of
    -- Special-casing on the booleans.
    Special UnitCon -> return (JsExpStmt forcedExp : body)
    UnQual "True"   -> boolIf True
    UnQual "False"  -> boolIf False
    -- Everything else, generic:
    _ -> do
      rf <- fmap (lookup cons) (gets stateRecords)
      let recordFields =
            fromMaybe
              (error $ "Constructor '" ++ prettyPrint cons ++
                       "' was not found in stateRecords, did you try running this through GHC first?")
              rf
      substmts <- foldM (\body (field,pat) ->
                             compilePat (JsGetProp forcedExp (JsNameVar field)) pat body)
                  body
                  (reverse (zip recordFields pats))
      qcons <- unsafeResolveName cons
      return [JsIf (forcedExp `JsInstanceOf` JsConstructor qcons)
                   substmts
                   []]

-- | Compile a pattern list.
compilePList :: [Pat] -> [JsStmt] -> JsExp -> Compile [JsStmt]
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
compileInfixPat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp left (Special cons) right) body =
  case cons of
    Cons -> do
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
