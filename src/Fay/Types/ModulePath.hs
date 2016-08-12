module Fay.Types.ModulePath
  ( ModulePath (..)
  , mkModulePath
  , mkModulePaths
  , mkModulePathFromQName
  ) where

import           Fay.Compiler.QName
import qualified Fay.Exts                        as F

import           Data.List
import           Data.List.Split
import           Language.Haskell.Exts

-- | The name of a module split into a list for code generation.
newtype ModulePath = ModulePath { unModulePath :: [String] }
  deriving (Eq, Ord, Show)

-- | Construct the complete ModulePath from a ModuleName.
mkModulePath :: ModuleName a -> ModulePath
mkModulePath (ModuleName _ m) = ModulePath . splitOn "." $ m

-- | Construct intermediate module paths from a ModuleName.
-- mkModulePaths "A.B" => [["A"], ["A","B"]]
mkModulePaths :: ModuleName a -> [ModulePath]
mkModulePaths (ModuleName _ m) = map ModulePath . tail . inits . splitOn "." $ m

-- | Converting a QName to a ModulePath is only relevant for constructors since
-- they can conflict with module names.
mkModulePathFromQName :: QName a -> ModulePath
mkModulePathFromQName (Qual _ (ModuleName _ m) n) = mkModulePath $ ModuleName F.noI $ m ++ "." ++ unname n
mkModulePathFromQName _ = error "mkModulePathFromQName: Not a qualified name"
