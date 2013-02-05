{-# LANGUAGE OverloadedStrings #-}

module Fay.Compiler.Defaults where

import Data.Default
import Data.Map as M
import Data.Set as S
import Fay.Compiler.Config
import Fay.Types
import Language.Haskell.Exts.Syntax
import Paths_fay

-- | The default compiler reader value.
defaultCompileReader :: CompileConfig -> IO CompileReader
defaultCompileReader config = do
  srcdir <- faySourceDir
  return CompileReader
    { readerConfig = addConfigDirectoryInclude Nothing srcdir config
    }

-- | The default compiler state.
defaultCompileState :: IO CompileState
defaultCompileState = do
  types <- getDataFileName "src/Language/Fay/Types.hs"
  return $ CompileState {
    _stateExports = M.empty
  , stateModuleName = ModuleName "Main"
  , stateRecordTypes = []
  , stateRecords = []
  , stateImported = [("Fay.Types",types)]
  , stateNameDepth = 1
  , stateFilePath = "<unknown>"
  , stateLocalScope = S.empty
  , stateModuleScope = def
  }
