{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Compile declarations.

module Fay.Compiler.Decl where

import Fay.Compiler.Exp
import Fay.Compiler.FFI
import Fay.Compiler.GADT
import Fay.Compiler.Misc
import Fay.Compiler.Pattern
import Fay.Data.List.Extra
import Fay.Compiler.ModuleScope (fieldDeclNames, convertFieldDecl)
import Fay.Types
import qualified Fay.Exts as F
import Fay.Exts.NoAnnotation (unAnn)
import Fay.Exts (noI)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Language.Haskell.Exts.Annotated

-- | Compile Haskell declaration.
compileDecls :: Bool -> [F.Decl] -> Compile [JsStmt]
compileDecls toplevel decls =
  case decls of
    [] -> return []
    (TypeSig _ _ sig:bind@PatBind{}:decls) -> appendM (scoped (compilePatBind toplevel (Just sig) bind))
                                                      (compileDecls toplevel decls)
    (decl:decls) -> appendM (scoped (compileDecl toplevel decl))
                            (compileDecls toplevel decls)

  where appendM m n = do x <- m
                         xs <- n
                         return (x ++ xs)
        scoped = if toplevel then withScope else id

-- | Compile a declaration.
compileDecl :: Bool -> F.Decl -> Compile [JsStmt]
compileDecl toplevel decl =
  case decl of
    pat@PatBind{} -> compilePatBind toplevel Nothing pat
    FunBind _ matches -> compileFunCase toplevel matches
--  DataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l        ) [QualConDecl l] (Maybe (Deriving l))
    DataDecl _ (DataType _ ) _                   head' constructors    _                     -> compileDataDecl toplevel (mkTyVars head') constructors
    GDataDecl _ (DataType _) _l (mkTyVars -> tyvars) _n decls _ -> compileDataDecl toplevel tyvars (map convertGADT decls)
    DataDecl _ (NewType _)  _ _ _ _ -> return []
    -- Just ignore type aliases and signatures.
    TypeDecl {} -> return []
    TypeSig  {} -> return []
    InfixDecl{} -> return []
    ClassDecl{} -> return []
    InstDecl {} -> return [] -- FIXME: Ignore.
    DerivDecl{} -> return []
    _ -> throwError (UnsupportedDeclaration decl)


mkTyVars :: F.DeclHead -> [F.TyVarBind]
mkTyVars (DHead _ _ binds) = binds
mkTyVars (DHInfix _ t1 _ t2) = [t1, t2]
mkTyVars (DHParen _ dh) = mkTyVars dh

-- | Convenient instance.
instance CompilesTo F.Decl [JsStmt] where compileTo = compileDecl True

-- | Compile a top-level pattern bind.
compilePatBind :: Bool -> Maybe F.Type -> F.Decl -> Compile [JsStmt]
compilePatBind toplevel sig pat =
  case pat of
    PatBind srcloc (PVar _ ident) Nothing (UnGuardedRhs _ rhs) Nothing ->
      case ffiExp rhs of
        Just formatstr -> case sig of
          Just sig -> compileFFI srcloc ident formatstr sig
          Nothing  -> throwError (FfiNeedsTypeSig pat)
        _ -> compileUnguardedRhs srcloc toplevel ident rhs
    PatBind srcloc (PVar _ ident) Nothing (UnGuardedRhs _ rhs) (Just bdecls) ->
      compileUnguardedRhs srcloc toplevel ident (Let noI bdecls rhs)
--    PatBind srcloc (PVar _ ident) Nothing (UnGuardedRhs _ rhs) Nothing ->
--      compileUnguardedRhs srcloc toplevel ident (Let noI (BDecls noI []) rhs)
    PatBind srcloc pat Nothing (UnGuardedRhs _ rhs) _bdecls -> do
      exp <- compileExp rhs
      name <- withScopedTmpJsName return
      [JsIf t b1 []] <- compilePat (JsName name) pat []
      let err = [throw (printSrcSpanInfo srcloc ++ "Irrefutable pattern failed for pattern: " ++ prettyPrint pat) (JsList [])]
      return [JsVar name exp, JsIf t b1 err]
    _ -> throwError (UnsupportedDeclaration pat)

-- | Compile a normal simple pattern binding.
compileUnguardedRhs :: SrcSpanInfo -> Bool -> F.Name -> F.Exp -> Compile [JsStmt]
compileUnguardedRhs _srcloc toplevel ident rhs = do
  unless toplevel $ bindVar ident
  withScope $ do
    body <- compileExp rhs
    bind <- bindToplevel toplevel ident (thunk body)
    return [bind]

-- | Compile a data declaration (or a GADT, latter is converted to former).
compileDataDecl :: Bool -> [F.TyVarBind] -> [F.QualConDecl] -> Compile [JsStmt]
compileDataDecl toplevel tyvars constructors =
  fmap concat $
    forM constructors $ \(QualConDecl srcloc _ _ condecl) ->
      case condecl of
        ConDecl _ name types -> do
          let slots =  map (Ident () . ("slot"++) . show . fst) $ zip [1 :: Int ..] types
              fields = zip (map return slots) types
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name tyvars fields
          emitJsToFay name tyvars fields
          emitCons cons
          return [func]
        InfixConDecl _ t1 name t2 -> do
          let slots = [Ident () "slot1",Ident () "slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name tyvars fields
          emitJsToFay name tyvars fields
          emitCons cons
          return [func]
        RecDecl _ name fields' -> do
          let fields = concatMap fieldDeclNames fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors srcloc fields
          emitFayToJs name tyvars (map convertFieldDecl fields')
          emitJsToFay name tyvars (map convertFieldDecl fields')
          emitCons cons
          return (func : funs)

  where
    emitCons cons = tell (mempty { writerCons = [cons] })

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
        then return . JsSetQName qname $ JsApp (JsName $ JsBuiltIn "objConcat")
                                               [func, JsName $ JsNameVar qname]
        else do
          modify $ addModulePath mp
          return $ JsSetQName qname func

    -- Creates getters for a RecDecl's values
    makeAccessors :: SrcSpanInfo -> [F.Name] -> Compile [JsStmt]
    makeAccessors _srcloc fields =
      forM fields $ \(unAnn -> name) ->
           bindToplevel toplevel
                        name
                        (JsFun Nothing
                               [JsNameVar "x"]
                               []
                               (Just (thunk (JsGetProp (force (JsName (JsNameVar "x")))
                                                       (JsNameVar (UnQual () name))))))


-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [F.Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel (InfixMatch l pat name pats rhs binds : rest) =
  compileFunCase toplevel (Match l name (pat:pats) rhs binds : rest)
compileFunCase toplevel matches@(Match _ name argslen _ _:_) = do
  pats <- fmap optimizePatConditions (mapM compileCase matches)
  bindVar name
  bind <- bindToplevel toplevel
                       name
                       (foldr (\arg inner -> JsFun Nothing [arg] [] (Just inner))
                              (stmtsThunk (concat pats ++ basecase))
                              args)
  return [bind]
  where args = zipWith const uniqueNames argslen

        isWildCardMatch (Match _ _ pats _ _) = all isWildCardPat pats

        compileCase :: F.Match -> Compile [JsStmt]
        compileCase (InfixMatch l pat name pats rhs binds) =
          compileCase $ Match l name (pat:pats) rhs binds
        compileCase match@(Match _ _ pats rhs _) =
          withScope $ do
            whereDecls' <- whereDecls match
            generateScope $ zipWithM (\arg pat -> compilePat (JsName arg) pat []) args pats
            generateScope $ mapM compileLetDecl whereDecls'
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

        whereDecls :: F.Match -> Compile [F.Decl]
        whereDecls (Match _ _ _ _ (Just (BDecls _ decls))) = return decls
        whereDecls (Match _ _ _ _ Nothing) = return []
        whereDecls match = throwError (UnsupportedWhereInMatch match)

        basecase :: [JsStmt]
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ prettyPrint name)
                                  (JsList (map JsName args))]

-- | Compile a right-hand-side expression.
compileRhs :: F.Rhs -> Compile (Either JsStmt JsExp)
compileRhs (UnGuardedRhs _ exp) = Right <$> compileExp exp
compileRhs (GuardedRhss _ rhss) = Left <$> compileGuards rhss
