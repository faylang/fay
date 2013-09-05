{-# LANGUAGE OverloadedStrings #-}

-- | Default values.

module Fay.Compiler.Defaults where

import           Fay.Compiler.Config
import           Fay.Compiler.Decl   (compileDecls)
import           Fay.Compiler.Exp    (compileLit)
import           Fay.Types
import           Paths_fay

import           Data.Map            as M
import           Data.Set            as S

-- | The data-files source directory.
faySourceDir :: IO FilePath
faySourceDir = getDataFileName "src/"

-- | The default compiler reader value.
defaultCompileReader :: CompileConfig -> IO CompileReader
defaultCompileReader config = do
  srcdir <- faySourceDir
  return CompileReader
    { readerConfig = addConfigDirectoryInclude Nothing srcdir config
    , readerCompileLit = compileLit
    , readerCompileDecls = compileDecls
    }

-- | The default compiler state.
defaultCompileState :: CompileState
defaultCompileState = CompileState
  { stateInterfaces    = M.empty
  , stateModuleName    = "Main"
  , stateRecordTypes   = []
  , stateRecords       = []
  , stateNewtypes      = []
  , stateImported      = []
  , stateNameDepth     = 1
  , stateJsModulePaths = S.empty
  , stateUseFromString = False
  , stateTypeSigs      = M.empty
  }
