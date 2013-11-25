{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Compile declarations.

module Fay.Compiler.Decl where

import           Fay.Compiler.Exp
import           Fay.Compiler.FFI
import           Fay.Compiler.GADT
import           Fay.Compiler.Misc
import           Fay.Compiler.Pattern
import           Fay.Compiler.State
import           Fay.Data.List.Extra
import           Fay.Exts                        (convertFieldDecl, fieldDeclNames)
import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.Scoped                 as S
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.RWS
import           Language.Haskell.Exts.Annotated

-- | Compile Haskell declaration.
compileDecls :: Bool -> [S.Decl] -> Compile [JsStmt]
compileDecls toplevel decls = case decls of
  [] -> return []
  (TypeSig _ _ sig:bind@PatBind{}:decls) -> appendM (compilePatBind toplevel (Just sig) bind)
                                                    (compileDecls toplevel decls)
  (decl:decls) -> appendM (compileDecl toplevel decl)
                          (compileDecls toplevel decls)

  where
    appendM m n = do x <- m
                     xs <- n
                     return (x ++ xs)

-- | Compile a declaration.
compileDecl :: Bool -> S.Decl -> Compile [JsStmt]
compileDecl toplevel decl = case decl of
  pat@PatBind{} -> compilePatBind toplevel Nothing pat
  FunBind _ matches -> compileFunCase toplevel matches
  DataDecl _ (DataType _ ) _ (mkTyVars -> tyvars) constructors _ -> compileDataDecl toplevel tyvars constructors
  GDataDecl _ (DataType _) _l (mkTyVars -> tyvars) _n decls _ -> compileDataDecl toplevel tyvars (map convertGADT decls)
  DataDecl _ (NewType _) _ _ _ _ -> return []
  -- Just ignore type aliases and signatures.
  TypeDecl {} -> return []
  TypeSig  {} -> return []
  InfixDecl{} -> return []
  ClassDecl{} -> return []
  InstDecl {} -> return [] -- FIXME: Ignore.
  DerivDecl{} -> return []
  DefaultDecl{} -> return []
  RulePragmaDecl{} -> return []
  DeprPragmaDecl{} -> return []
  WarnPragmaDecl{} -> return []
  InlineSig{} -> return []
  InlineConlikeSig{} -> return []
  SpecSig{} -> return []
  SpecInlineSig{} -> return []
  InstSig{} -> return []
  AnnPragma{} -> return []
  _ -> throwError (UnsupportedDeclaration decl)


mkTyVars :: S.DeclHead -> [S.TyVarBind]
mkTyVars (DHead _ _ binds) = binds
mkTyVars (DHInfix _ t1 _ t2) = [t1, t2]
mkTyVars (DHParen _ dh) = mkTyVars dh

-- | Compile a top-level pattern bind.
compilePatBind :: Bool -> Maybe S.Type -> S.Decl -> Compile [JsStmt]
compilePatBind toplevel sig patDecl = case patDecl of
  PatBind srcloc (PVar _ ident) Nothing (UnGuardedRhs _ rhs) Nothing ->
    case ffiExp rhs of
      Just formatstr -> case sig of
        Just sig -> compileFFI ident formatstr sig
        Nothing  -> throwError $ FfiNeedsTypeSig patDecl
      _ -> compileUnguardedRhs toplevel srcloc ident rhs
  -- TODO: Generalize to all patterns
  PatBind srcloc (PVar _ ident) Nothing (UnGuardedRhs _ rhs) (Just bdecls) ->
    compileUnguardedRhs toplevel srcloc ident (Let S.noI bdecls rhs)
  PatBind _ pat Nothing (UnGuardedRhs _ rhs) _bdecls -> case pat of
    PList {} -> compilePatBind' pat rhs
    PTuple{} -> compilePatBind' pat rhs
    PApp  {} -> compilePatBind' pat rhs
    _        -> throwError $ UnsupportedDeclaration patDecl
  _ -> throwError $ UnsupportedDeclaration patDecl
  where
    compilePatBind' :: S.Pat -> S.Exp -> Compile [JsStmt]
    compilePatBind' pat rhs = do
      exp <- compileExp rhs
      name <- withScopedTmpJsName return
      [JsIf t b1 []] <- compilePat (JsName name) pat []
      let err = [throw ("Irrefutable pattern failed for pattern: " ++ prettyPrint pat) (JsList [])]
      return [JsVar name exp, JsIf t b1 err]

-- | Compile a normal simple pattern binding.
compileUnguardedRhs :: Bool -> S.X -> S.Name -> S.Exp -> Compile [JsStmt]
compileUnguardedRhs toplevel srcloc ident rhs = do
  body <- compileExp rhs
  bind <- bindToplevel toplevel (Just (srcInfoSpan (S.srcSpanInfo srcloc))) ident (thunk body)
  return [bind]

-- | Compile a data declaration (or a GADT, latter is converted to former).
compileDataDecl :: Bool -> [S.TyVarBind] -> [S.QualConDecl] -> Compile [JsStmt]
compileDataDecl toplevel tyvars constructors =
  fmap concat $
    forM constructors $ \(QualConDecl _ _ _ condecl) ->
      case condecl of
        ConDecl _ name types -> do
          let slots =  map (Ident () . ("slot"++) . show . fst) $ zip [1 :: Int ..] types
              fields = zip (map return slots) types
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name tyvars fields
          emitJsToFay name tyvars fields
          return [cons, func]
        InfixConDecl _ t1 name t2 -> do
          let slots = [Ident () "slot1",Ident () "slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name tyvars fields
          emitJsToFay name tyvars fields
          return [cons, func]
        RecDecl _ name fields' -> do
          let fields = concatMap fieldDeclNames fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors fields
          emitFayToJs name tyvars (map convertFieldDecl fields')
          emitJsToFay name tyvars (map convertFieldDecl fields')
          return (cons : func : funs)

  where
    -- Creates a constructor _RecConstr for a Record
    makeConstructor :: Name a -> [Name b] -> Compile JsStmt
    makeConstructor (unAnn -> name) (map (JsNameVar . UnQual () . unAnn) -> fields) = do
      qname <- qualify name
      return $
        JsSetConstructor qname $
          JsFun (Just $ JsConstructor qname)
                fields
                (for fields $ \field -> JsSetProp JsThis field (JsName field))
                Nothing

    -- Creates a function to initialize the record by regular application
    makeFunc :: Name a -> [Name b] -> Compile JsStmt
    makeFunc (unAnn -> name) (map (JsNameVar . UnQual () . unAnn) -> fields) = do
      let fieldExps = map JsName fields
      qname <- qualify name
      let mp = mkModulePathFromQName qname
      let func = foldr (\slot inner -> JsFun Nothing [slot] [] (Just inner))
                       (thunk $ JsNew (JsConstructor qname) fieldExps)
                       fields
      added <- gets (addedModulePath mp)
      if added
        then return . JsSetQName Nothing qname $ JsApp (JsName $ JsBuiltIn "objConcat")
                                                       [func, JsName $ JsNameVar qname]
        else do
          modify $ addModulePath mp
          return $ JsSetQName Nothing qname func

    -- Creates getters for a RecDecl's values
    makeAccessors :: [S.Name] -> Compile [JsStmt]
    makeAccessors fields =
      forM fields $ \(unAnn -> name) ->
           bindToplevel toplevel
                        Nothing
                        name
                        (JsFun Nothing
                               [JsNameVar "x"]
                               []
                               (Just (thunk (JsGetProp (force (JsName (JsNameVar "x")))
                                                       (JsNameVar (UnQual () name))))))


-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [S.Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel (InfixMatch l pat name pats rhs binds : rest) =
  compileFunCase toplevel (Match l name (pat:pats) rhs binds : rest)
compileFunCase toplevel matches@(Match srcloc name argslen _ _:_) = do
  pats <- fmap optimizePatConditions (mapM compileCase matches)
  bind <- bindToplevel toplevel
                       (Just (srcInfoSpan (S.srcSpanInfo srcloc)))
                       name
                       (foldr (\arg inner -> JsFun Nothing [arg] [] (Just inner))
                              (stmtsThunk (concat pats ++ basecase))
                              args)
  return [bind]
  where
    args = zipWith const uniqueNames argslen

    isWildCardMatch (Match _ _ pats          _ _) = all isWildCardPat pats
    isWildCardMatch (InfixMatch _ pat _ pats _ _) = all isWildCardPat (pat:pats)

    compileCase :: S.Match -> Compile [JsStmt]
    compileCase (InfixMatch l pat name pats rhs binds) =
      compileCase $ Match l name (pat:pats) rhs binds
    compileCase match@(Match _ _ pats rhs _) = do
      whereDecls' <- whereDecls match
      rhsform <- compileRhs rhs
      body <- if null whereDecls'
                then return [either id JsEarlyReturn rhsform]
                else do
                    binds <- mapM compileLetDecl whereDecls'
                    case rhsform of
                      Right exp ->
                        return [JsEarlyReturn $ JsApp (JsFun Nothing [] (concat binds) (Just exp)) []]
                      Left stmt ->
                        withScopedTmpJsName $ \n -> return
                          [ JsVar n (JsApp (JsFun Nothing [] (concat binds ++ [stmt]) Nothing) [])
                          , JsIf (JsNeq JsUndefined (JsName n)) [JsEarlyReturn (JsName n)] []
                          ]
      foldM (\inner (arg,pat) ->
              compilePat (JsName arg) pat inner)
            body
            (zip args pats)

    whereDecls :: S.Match -> Compile [S.Decl]
    whereDecls (Match _ _ _ _ (Just (BDecls _ decls))) = return decls
    whereDecls (Match _ _ _ _ Nothing) = return []
    whereDecls match = throwError (UnsupportedWhereInMatch match)

    basecase :: [JsStmt]
    basecase = if any isWildCardMatch matches
                  then []
                  else [throw ("unhandled case in " ++ prettyPrint name)
                              (JsList (map JsName args))]

-- | Compile a right-hand-side expression.
compileRhs :: S.Rhs -> Compile (Either JsStmt JsExp)
compileRhs (UnGuardedRhs _ exp) = Right <$> compileExp exp
compileRhs (GuardedRhss _ rhss) = Left <$> compileGuards rhss
