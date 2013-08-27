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
import Fay.Types

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Language.Haskell.Exts

-- | Compile Haskell declaration.
compileDecls :: Bool -> [Decl] -> Compile [JsStmt]
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
compileDecl :: Bool -> Decl -> Compile [JsStmt]
compileDecl toplevel decl =
  case decl of
    pat@PatBind{} -> compilePatBind toplevel Nothing pat
    FunBind matches -> compileFunCase toplevel matches
    DataDecl _ DataType _ _ tyvars constructors _ -> compileDataDecl toplevel tyvars constructors
    GDataDecl _ DataType _l _i tyvars _n decls _ -> compileDataDecl toplevel tyvars (map convertGADT decls)
    DataDecl _ NewType  _ _ _ _ _ -> return []
    -- Just ignore type aliases and signatures.
    TypeDecl {} -> return []
    TypeSig  {} -> return []
    InfixDecl{} -> return []
    ClassDecl{} -> return []
    InstDecl {} -> return [] -- FIXME: Ignore.
    DerivDecl{} -> return []
    _ -> throwError (UnsupportedDeclaration decl)

-- | Convenient instance.
instance CompilesTo Decl [JsStmt] where compileTo = compileDecl True

-- | Compile a top-level pattern bind.
compilePatBind :: Bool -> Maybe Type -> Decl -> Compile [JsStmt]
compilePatBind toplevel sig pat =
  case pat of
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) (BDecls []) ->
      case ffiExp rhs of
        Just formatstr -> case sig of
          Just sig -> compileFFI srcloc ident formatstr sig
          Nothing  -> throwError (FfiNeedsTypeSig pat)
        _ -> compileUnguardedRhs srcloc toplevel ident rhs
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) bdecls ->
      compileUnguardedRhs srcloc toplevel ident (Let bdecls rhs)
    PatBind srcloc pat Nothing (UnGuardedRhs rhs) _bdecls -> do
      exp <- compileExp rhs
      name <- withScopedTmpJsName return
      [JsIf t b1 []] <- compilePat (JsName name) pat []
      let err = [throw (prettyPrint srcloc ++ "Irrefutable pattern failed for pattern: " ++ prettyPrint pat) (JsList [])]
      return [JsVar name exp, JsIf t b1 err]
    _ -> throwError (UnsupportedDeclaration pat)

-- | Compile a normal simple pattern binding.
compileUnguardedRhs :: SrcLoc -> Bool -> Name -> Exp -> Compile [JsStmt]
compileUnguardedRhs _srcloc toplevel ident rhs = do
  unless toplevel $ bindVar ident
  withScope $ do
    body <- compileExp rhs
    bind <- bindToplevel toplevel ident (thunk body)
    return [bind]

-- | Compile a data declaration (or a GADT, latter is converted to former).
compileDataDecl :: Bool -> [TyVarBind] -> [QualConDecl] -> Compile [JsStmt]
compileDataDecl toplevel tyvars constructors =
  fmap concat $
    forM constructors $ \(QualConDecl srcloc _ _ condecl) ->
      case condecl of
        ConDecl name types  -> do
          let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
              fields' = zip (map return fields) types
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          emitFayToJs name tyvars fields'
          emitJsToFay name tyvars fields'
          emitCons cons
          return [func]
        InfixConDecl t1 name t2 -> do
          let slots = ["slot1","slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name tyvars fields
          emitJsToFay name tyvars fields
          emitCons cons
          return [func]
        RecDecl name fields' -> do
          let fields = concatMap fst fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors srcloc fields
          emitFayToJs name tyvars fields'
          emitJsToFay name tyvars fields'
          emitCons cons
          return (func : funs)

  where
    emitCons cons = tell (mempty { writerCons = [cons] })

    -- Creates a constructor _RecConstr for a Record
    makeConstructor :: Name -> [Name] -> Compile JsStmt
    makeConstructor name (map (JsNameVar . UnQual) -> fields) = do
      qname <- qualify name
      return $
        JsSetConstructor qname $
          JsFun (Just $ JsConstructor qname)
                fields
                (for fields $ \field -> JsSetProp JsThis field (JsName field))
                Nothing

    -- Creates a function to initialize the record by regular application
    makeFunc :: Name -> [Name] -> Compile JsStmt
    makeFunc name (map (JsNameVar . UnQual) -> fields) = do
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
    makeAccessors :: SrcLoc -> [Name] -> Compile [JsStmt]
    makeAccessors _srcloc fields =
      forM fields $ \name ->
           bindToplevel toplevel
                        name
                        (JsFun Nothing
                               [JsNameVar "x"]
                               []
                               (Just (thunk (JsGetProp (force (JsName (JsNameVar "x")))
                                                       (JsNameVar (UnQual name))))))


-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel matches@(Match _ name argslen _ _ _:_) = do
  pats <- fmap optimizePatConditions (mapM compileCase matches)
  bindVar name
  bind <- bindToplevel toplevel
                       name
                       (foldr (\arg inner -> JsFun Nothing [arg] [] (Just inner))
                              (stmtsThunk (concat pats ++ basecase))
                              args)
  return [bind]
  where args = zipWith const uniqueNames argslen

        isWildCardMatch (Match _ _ pats _ _ _) = all isWildCardPat pats

        compileCase :: Match -> Compile [JsStmt]
        compileCase match@(Match _ _ pats _ rhs _) =
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

        whereDecls :: Match -> Compile [Decl]
        whereDecls (Match _ _ _ _ _ (BDecls decls)) = return decls
        whereDecls match = throwError (UnsupportedWhereInMatch match)

        basecase :: [JsStmt]
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ prettyPrint name)
                                  (JsList (map JsName args))]

-- | Compile a right-hand-side expression.
compileRhs :: Rhs -> Compile (Either JsStmt JsExp)
compileRhs (UnGuardedRhs exp) = Right <$> compileExp exp
compileRhs (GuardedRhss rhss) = Left <$> compileGuards rhss
