-- | Generate names while desugaring.
module Fay.Compiler.Desugar.Name
  ( withScopedTmpName
  , unscopedTmpNames
  ) where

import           Fay.Compiler.Prelude

import           Fay.Compiler.Desugar.Types

import           Control.Monad.Reader            (asks, local)
import           Language.Haskell.Exts (Name (..))

-- | Generate a temporary, SCOPED name for testing conditions and
-- such. We don't have name tracking yet, so instead we use this.
withScopedTmpName :: (Data l, Typeable l) => l -> (Name l -> Desugar l a) -> Desugar l a
withScopedTmpName l f = do
  prefix <- asks readerTmpNamePrefix
  n <- asks readerNameDepth
  local (\r -> r { readerNameDepth = n + 1 }) $
    f $ tmpName l prefix n

-- | Generates temporary names where the scope doesn't matter.
unscopedTmpNames :: l -> String -> [Name l]
unscopedTmpNames l prefix = map (tmpName l prefix) [0..]

-- | Don't call this directly, use withScopedTmpName or unscopedTmpNames instead.
tmpName :: l -> String -> Int -> Name l
tmpName l prefix n = Ident l $ prefix ++ show n
