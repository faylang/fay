-- | This module has been deprecated in favor of Fay.Config

module Fay.Compiler.Config
  {-# DEPRECATED "Module has moved to Fay.Config and CompileConfig has been renamed to Config. This module will be removed in a later major release." #-}
  ( module Fay.Config
  , CompileConfig
  ) where

import           Fay.Config

type CompileConfig = Config
