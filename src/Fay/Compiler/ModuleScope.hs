{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Handles variable bindings on the module level and also keeps track of
-- primitive operations that we want to treat specially.

module Fay.Compiler.ModuleScope
  (ModuleScope
  ,bindAsLocals
  ,moduleLocals
  ,findPrimOp
  ,convertFieldDecl -- TODO temp location
  ,fieldDeclNames -- TODO temp location
  ,resolvePrimOp
  ) where

import           Fay.Compiler.QName
import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.NoAnnotation           as N

import           Control.Arrow
import           Control.Monad.Writer
import           Data.Default
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Language.Haskell.Exts.Annotated hiding (binds, name)
import           Prelude                         hiding (mod)

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
-- TODO for now add Symbol for the symbols ALSO, or just dont create the incorrect one. Fixes Eq.hs test
envPrimOpsMap = M.fromList
  [ (Symbol () ">>",     Qual () (ModuleName () "Fay$") (Ident () "then"))
  , (Symbol () ">>=",    Qual () (ModuleName () "Fay$") (Ident () "bind"))
  , (Ident  () "return", Qual () (ModuleName () "Fay$") (Ident () "return"))
  , (Ident  () "force",  Qual () (ModuleName () "Fay$") (Ident () "force"))
  , (Ident  () "seq",    Qual () (ModuleName () "Fay$") (Ident () "seq"))
  , (Symbol ()  "*",     Qual () (ModuleName () "Fay$") (Ident () "mult"))
  , (Symbol ()  "+",     Qual () (ModuleName () "Fay$") (Ident () "add"))
  , (Symbol ()  "-",     Qual () (ModuleName () "Fay$") (Ident () "sub"))
  , (Symbol ()  "/",     Qual () (ModuleName () "Fay$") (Ident () "divi"))
  , (Symbol ()  "==",    Qual () (ModuleName () "Fay$") (Ident () "eq"))
  , (Symbol ()  "/=",    Qual () (ModuleName () "Fay$") (Ident () "neq"))
  , (Symbol ()  ">",     Qual () (ModuleName () "Fay$") (Ident () "gt"))
  , (Symbol ()  "<",     Qual () (ModuleName () "Fay$") (Ident () "lt"))
  , (Symbol ()  ">=",    Qual () (ModuleName () "Fay$") (Ident () "gte"))
  , (Symbol ()  "<=",    Qual () (ModuleName () "Fay$") (Ident () "lte"))
  , (Symbol ()  "&&",    Qual () (ModuleName () "Fay$") (Ident () "and"))
  , (Symbol ()  "||",    Qual () (ModuleName () "Fay$") (Ident () "or"))
  ]

-- | Lookup a primop that was resolved to a Prelude definition.
findPrimOp :: N.QName -> Maybe N.QName
findPrimOp (Qual _ (ModuleName _ "Prelude") s) = M.lookup s envPrimOpsMap
findPrimOp _ = Nothing

resolvePrimOp :: QName a -> Maybe N.QName
resolvePrimOp (unAnn -> q) = case q of
  (Qual _ (ModuleName _ "Prelude") _) -> findPrimOp q
  (UnQual _ n) -> findPrimOp $ Qual () (ModuleName () "Prelude") n
  _ -> Nothing



--------------------------------------------------------------------------------
-- AST

-- TODO move
convertFieldDecl :: FieldDecl a -> ([Name a], BangType a)
convertFieldDecl (FieldDecl _ ns b) = (ns, b)

fieldDeclNames :: FieldDecl a -> [Name a]
fieldDeclNames (FieldDecl _ ns _) = ns
