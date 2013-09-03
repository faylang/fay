{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

--------------------------------------------------------------------------------
-- | Primitive Operations
-- Built-in operations that aren't actually compiled from
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

module Fay.Compiler.PrimOp
  ( fayBuiltin
  , findPrimOp
  , resolvePrimOp
  ) where

import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.NoAnnotation           as N

import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Language.Haskell.Exts.Annotated hiding (binds, name)
import           Prelude                         hiding (mod)

-- | Make an identifier from the built-in HJ module.
fayBuiltin :: a -> String -> QName a
fayBuiltin a = Qual a (ModuleName a "Fay$") . Ident a

-- | Mapping from unqualified names to qualified primitive names.
primOpsMap :: Map N.Name N.QName
primOpsMap = M.fromList
  [ (Symbol () ">>",     fayBuiltin () "then")
  , (Symbol () ">>=",    fayBuiltin () "bind")
  , (Ident  () "return", fayBuiltin () "return")
  , (Ident  () "force",  fayBuiltin () "force")
  , (Ident  () "seq",    fayBuiltin () "seq")
  , (Symbol ()  "*",     fayBuiltin () "mult")
  , (Symbol ()  "+",     fayBuiltin () "add")
  , (Symbol ()  "-",     fayBuiltin () "sub")
  , (Symbol ()  "/",     fayBuiltin () "divi")
  , (Symbol ()  "==",    fayBuiltin () "eq")
  , (Symbol ()  "/=",    fayBuiltin () "neq")
  , (Symbol ()  ">",     fayBuiltin () "gt")
  , (Symbol ()  "<",     fayBuiltin () "lt")
  , (Symbol ()  ">=",    fayBuiltin () "gte")
  , (Symbol ()  "<=",    fayBuiltin () "lte")
  , (Symbol ()  "&&",    fayBuiltin () "and")
  , (Symbol ()  "||",    fayBuiltin () "or")
  ]

-- | Lookup a primop that was resolved to a Prelude definition.
findPrimOp :: N.QName -> Maybe N.QName
findPrimOp (Qual _ (ModuleName _ "Prelude") s) = M.lookup s primOpsMap
findPrimOp _ = Nothing

-- | If this is resolved to a Prelude identifier or if it's unqualified,
-- check if it's a primop
resolvePrimOp :: QName a -> Maybe N.QName
resolvePrimOp (unAnn -> q) = case q of
  (Qual _ (ModuleName _ "Prelude") _) -> findPrimOp q
  (UnQual _ n) -> findPrimOp $ Qual () (ModuleName () "Prelude") n
  _ -> Nothing
