{-# LANGUAGE OverloadedStrings #-}

-- | Default values.

module Fay.Compiler.Defaults where

import Fay.Compiler.Config
import Fay.Compiler.Decl (compileDecls)
import Fay.Compiler.Exp (compileLit)
import Fay.Types

import Data.Default
import Data.Map as M
import Data.Set as S
import Language.Haskell.Exts.Syntax
import Paths_fay

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
defaultCompileState :: IO CompileState
defaultCompileState = do
  types <- getDataFileName "src/Language/Fay/Types.hs"
  return $ CompileState {
    _stateExports = M.empty
  , stateModuleName = ModuleName "Main"
  , stateRecordTypes = []
  , stateRecords = []
  , stateNewtypes = []
  , stateImported = [("Fay.Types",types)]
  , stateNameDepth = 1
  , stateLocalScope = S.empty
  , stateModuleScope = def
  }
