{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Handles variable bindings on the module level and also keeps track of
-- primitive operations that we want to treat specially.

module Fay.Compiler.ModuleScope
  (ModuleScope
  ,bindAsLocals
  ,findTopLevelNames
  ,moduleLocals
  ,findPrimOp
  ,convertFieldDecl -- TODO temp location
  ,fieldDeclNames -- TODO temp location
  ,resolvePrimOp
  ) where

import Debug.Trace

import           Fay.Compiler.GADT
import qualified Fay.Exts as F
import qualified Fay.Exts.NoAnnotation as N
import Fay.Exts.NoAnnotation (unAnn)
import Fay.Compiler.QName

import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Language.Haskell.Exts.Annotated hiding (name, binds)
import           Prelude hiding (mod)

-- | Maps names bound in the module to their real names
-- The keys are unqualified for locals and imports,
-- the values are always fully qualified
-- Example contents:
--   [ (UnQUal     "main"      , Qual "Main"     "main")
--   , (UnQual     "take"      , Qual "Prelude"  "take")
--   , (  Qual "M" "insertWith", Qual "Data.Map" "insertWith") ]
newtype ModuleScope = ModuleScope (Map N.QName N.QName)
  deriving Show

instance Monoid ModuleScope where
  mempty                                  = ModuleScope M.empty
  mappend (ModuleScope a) (ModuleScope b) = ModuleScope $ a `M.union` b

instance Default ModuleScope where
  def = mempty

-- | Bind a list of names into the local scope
-- Right now all bindings are made unqualified
bindAsLocals :: [N.QName] -> ModuleScope -> ModuleScope
bindAsLocals qs (ModuleScope binds) =
  -- This needs to be changed to not use unqual to support qualified imports.
  ModuleScope $ binds `M.union` M.fromList (map (unQualify &&& id) qs)

-- | Find all names that are bound locally in this module, which excludes imports.
moduleLocals :: N.ModuleName -> ModuleScope -> [N.QName]
moduleLocals mod (ModuleScope binds) = filter isLocal . M.elems $ binds
  where
    isLocal (Qual _ m _) = mod == m
    isLocal _ = False

--------------------------------------------------------------------------------
-- Primitive Operations

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
envPrimOpsMap :: Map N.Name N.QName
-- TODO These should not be Ident
envPrimOpsMap = M.fromList
  [ (Ident () ">>",     Qual () (ModuleName () "Fay$") (Ident () "then"))
  , (Ident () ">>=",    Qual () (ModuleName () "Fay$") (Ident () "bind"))
  , (Ident () "return", Qual () (ModuleName () "Fay$") (Ident () "return"))
  , (Ident () "force",  Qual () (ModuleName () "Fay$") (Ident () "force"))
  , (Ident () "seq",    Qual () (ModuleName () "Fay$") (Ident () "seq"))
  , (Ident ()  "*",     Qual () (ModuleName () "Fay$") (Ident () "mult"))
  , (Ident ()  "+",     Qual () (ModuleName () "Fay$") (Ident () "add"))
  , (Ident ()  "-",     Qual () (ModuleName () "Fay$") (Ident () "sub"))
  , (Ident ()  "/",     Qual () (ModuleName () "Fay$") (Ident () "divi"))
  , (Ident ()  "==",    Qual () (ModuleName () "Fay$") (Ident () "eq"))
  , (Ident ()  "/=",    Qual () (ModuleName () "Fay$") (Ident () "neq"))
  , (Ident ()  ">",     Qual () (ModuleName () "Fay$") (Ident () "gt"))
  , (Ident ()  "<",     Qual () (ModuleName () "Fay$") (Ident () "lt"))
  , (Ident ()  ">=",    Qual () (ModuleName () "Fay$") (Ident () "gte"))
  , (Ident ()  "<=",    Qual () (ModuleName () "Fay$") (Ident () "lte"))
  , (Ident ()  "&&",    Qual () (ModuleName () "Fay$") (Ident () "and"))
  , (Ident ()  "||",    Qual () (ModuleName () "Fay$") (Ident () "or"))
  ]

-- | Lookup a primop that was resolved to a Prelude definition.
findPrimOp :: N.QName -> Maybe N.QName
findPrimOp q@(Qual _ (ModuleName _ "Prelude") s) = M.lookup s envPrimOpsMap
findPrimOp _ = Nothing

resolvePrimOp :: QName a -> Maybe N.QName
resolvePrimOp (unAnn -> q) = case q of
  (Qual _ (ModuleName _ "Prelude") _) -> findPrimOp q
  (UnQual _ n) -> findPrimOp $ Qual () (ModuleName () "Prelude") n
  _ -> Nothing



--------------------------------------------------------------------------------
-- AST

type ModuleScopeSt = ReaderT F.ModuleName (Writer ModuleScope) ()

-- | Get module level names from a haskell module AST.
findTopLevelNames :: F.ModuleName -> [F.Decl] -> ModuleScope
findTopLevelNames mod decls = snd . runWriter $ runReaderT (mapM_ d_decl decls) mod

bindName :: F.Name -> ModuleScopeSt
bindName (unAnn -> k) = ask >>= \(unAnn -> mod) -> tell (ModuleScope $ M.singleton (UnQual () k) (Qual () mod k))

d_decl :: F.Decl -> ModuleScopeSt
d_decl d = case d of
  DataDecl _ _ _ _ dd _               -> mapM_ d_qualCon dd
  GDataDecl _ (DataType _) _ _ _ ds _ -> mapM_ (d_qualCon . convertGADT) ds
  PatBind _ (PVar _ n) _ _ _          -> bindName n
  FunBind _ (Match _ n _ _ _ : _)     -> bindName n
  ClassDecl _ _ _ _ (Just cds)        -> mapM_ d_classDecl cds
  ClassDecl _ _ _ _ Nothing           -> return ()
  TypeSig _ ns _                      -> mapM_ bindName ns
  _                                   -> return ()

d_classDecl :: F.ClassDecl -> ModuleScopeSt
d_classDecl cd = case cd of
  ClsDecl _ d -> d_decl d
  _         -> return ()

d_qualCon :: F.QualConDecl -> ModuleScopeSt
d_qualCon (QualConDecl _ _ _ cd) = case cd of
  ConDecl _ n _        -> bindName n
  InfixConDecl _ _ n _ -> bindName n
  RecDecl _ n ns       -> bindName n >> mapM_ bindName (concatMap fieldDeclNames ns)

-- TODO move
convertFieldDecl :: FieldDecl a -> ([Name a], BangType a)
convertFieldDecl (FieldDecl _ ns b) = (ns, b)

fieldDeclNames :: FieldDecl a -> [Name a]
fieldDeclNames (FieldDecl _ ns _) = ns
