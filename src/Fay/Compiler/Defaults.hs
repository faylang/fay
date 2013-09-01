{-# LANGUAGE OverloadedStrings #-}

-- | Default values.

module Fay.Compiler.Defaults where

import           Fay.Compiler.Config
import           Fay.Compiler.Decl   (compileDecls)
import           Fay.Compiler.Exp    (compileLit)
import           Fay.Types

import           Data.Default
import           Data.Map            as M
import           Data.Set            as S

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
-- TODO purify
defaultCompileState :: IO CompileState
defaultCompileState = do
  return CompileState
    {  _stateExports = M.empty
    , stateInterfaces = M.empty
    , stateModuleName = "Main"
    , stateRecordTypes = []
    , stateRecords = []
    , stateNewtypes = []
    , stateImported = []
    , stateNameDepth = 1
    , stateLocalScope = S.empty
    , stateModuleScope = def
    , stateModuleScopes = M.empty
    , stateJsModulePaths = S.empty
    , stateUseFromString = False
    }
