{-# LANGUAGE OverloadedStrings #-}

module Language.Fay.ModuleScope (ModuleScope, resolveName, findTopLevelNames, addExports) where

import           Control.Arrow
import           Control.Monad.State
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Language.Haskell.Exts hiding (name, binds)
import           Prelude hiding (mod, lookup)

newtype ModuleScope = ModuleScope (Map QName QName)
  deriving Show

instance Default ModuleScope where
  def = ModuleScope M.empty

lookup :: QName -> ModuleScope -> Maybe QName
lookup q (ModuleScope binds) = M.lookup q binds

-- | The built-in operations that aren't actually compiled from
-- anywhere, they come from runtime.js.
--
-- They're in the names list so that they can be overriden by the user
-- in e.g. let a * b = a - b in 1 * 2.
--
-- So we resolve them to Fay$, i.e. the prefix used for the runtime
-- support. $ is not allowed in Haskell module names, so there will be
-- no conflicts if a user decicdes to use a module named Fay.
--
-- So e.g. will compile to (*) Fay$$mult, which is in runtime.js.
type PrimModuleScope = Map Name QName
envPrimOpsMap :: PrimModuleScope
envPrimOpsMap = M.fromList
  [ (Symbol ">>",    (Qual (ModuleName "Fay$") (Ident "then")))
  , (Symbol ">>=",   (Qual (ModuleName "Fay$") (Ident "bind")))
  , (Ident "return", (Qual (ModuleName "Fay$") (Ident "return")))
  , (Ident "force",  (Qual (ModuleName "Fay$") (Ident "force")))
  , (Ident "seq",    (Qual (ModuleName "Fay$") (Ident "seq")))
  , (Symbol "*",     (Qual (ModuleName "Fay$") (Ident "mult")))
  , (Symbol "+",     (Qual (ModuleName "Fay$") (Ident "add")))
  , (Symbol "-",     (Qual (ModuleName "Fay$") (Ident "sub")))
  , (Symbol "/",     (Qual (ModuleName "Fay$") (Ident "div")))
  , (Symbol "==",    (Qual (ModuleName "Fay$") (Ident "eq")))
  , (Symbol "/=",    (Qual (ModuleName "Fay$") (Ident "neq")))
  , (Symbol ">",     (Qual (ModuleName "Fay$") (Ident "gt")))
  , (Symbol "<",     (Qual (ModuleName "Fay$") (Ident "lt")))
  , (Symbol ">=",    (Qual (ModuleName "Fay$") (Ident "gte")))
  , (Symbol "<=",    (Qual (ModuleName "Fay$") (Ident "lte")))
  , (Symbol "&&",    (Qual (ModuleName "Fay$") (Ident "and")))
  , (Symbol "||",    (Qual (ModuleName "Fay$") (Ident "or")))
  ]

findTopLevelNames :: ModuleName -> [Decl] -> ModuleScope
findTopLevelNames mod decls = snd $ execState (mapM_ d_decl decls) (mod,def)

type ModuleScopeSt = State (ModuleName, ModuleScope) ()

bindName :: Name -> ModuleScopeSt
bindName k = modify $ \st -> case st of
  (mod, ModuleScope binds) -> (mod, ModuleScope $ M.insert (UnQual k) (Qual mod k) binds)

d_decl :: Decl -> ModuleScopeSt
d_decl d = case d of
  DataDecl _ _ _ _ _ dd _         -> mapM_ d_qualCon dd
  PatBind _ (PVar n) _ _ _        -> bindName n
  FunBind (Match _ n _ _ _ _ : _) -> bindName n
  ClassDecl _ _ _ _ _ cds         -> mapM_ d_classDecl cds
  TypeSig _ ns _                  -> mapM_ bindName ns
  _                               -> return ()

d_classDecl :: ClassDecl -> ModuleScopeSt
d_classDecl cd = case cd of
  ClsDecl d -> d_decl d
  _         -> return ()

d_qualCon :: QualConDecl -> ModuleScopeSt
d_qualCon (QualConDecl _ _ _ cd) = case cd of
  ConDecl n _        -> bindName n
  InfixConDecl _ n _ -> bindName n
  RecDecl n ns       -> bindName n >> mapM_ bindName (concatMap fst ns)


resolveName :: QName -> ModuleScope -> Maybe QName
resolveName q env = case lookup q env of -- lookup in the module environment.

  -- something pointing to prelude, is it a primop?
  Just q'@(Qual (ModuleName "Prelude") n) -> case M.lookup n envPrimOpsMap of
    Just x  -> Just x  -- A primop which looks like it's imported from prelude.
    Nothing -> Just q' -- Regular prelude import, leave it as is.

  -- No matches in the current environment, so it may be a primop if it's unqualified.
  -- If Nothing is returned from either of the branches it means that there is
  -- no primop and nothing in env scope so GHC would have given an error.
  Nothing -> case q of
    UnQual n -> M.lookup n envPrimOpsMap
    _        -> Nothing
  j -> j -- Non-prelude import that was found in the env


addExports :: [QName] -> ModuleScope -> ModuleScope
addExports qs (ModuleScope binds) =
  -- this needs to be changed to not use unqual to support qualified imports
  ModuleScope $ M.fromList (map (unqual &&& id) qs) `M.union` binds
    where unqual (Qual _ n) = (UnQual n)
          unqual u@UnQual{} = u
          unqual Special{} = error "LOL"
