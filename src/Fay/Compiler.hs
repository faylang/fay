{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | The Haskell→Javascript compiler.

module Fay.Compiler
  (runCompile
  ,compileViaStr
  ,compileToAst
  ,compileExp
  ,compileDecl
  ,compileToplevelModule
  ,compileModuleFromFile
  ,compileModuleFromContents
  ,compileModuleFromName
  ,compileModule
  ,compileModuleFromAST
  ,parseFay)
  where

import           Fay.Compiler.Config
import           Fay.Compiler.Decl
import           Fay.Compiler.Defaults
import           Fay.Compiler.Exp
import           Fay.Compiler.FFI
import           Fay.Compiler.InitialPass (initialPass)
import           Fay.Compiler.Misc
import           Fay.Compiler.ModuleScope (findPrimOp)
import           Fay.Compiler.Optimizer
import           Fay.Compiler.Typecheck
import           Fay.Compiler.QName
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Control.Monad.RWS
import           Data.Default                    (def)
import qualified Data.Map                        as M
import           Data.Maybe
import qualified Data.Set                        as S
import           Language.Haskell.Exts

--------------------------------------------------------------------------------
-- Top level entry points

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: (Show from,Show to,CompilesTo from to)
              => FilePath
              -> CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (PrintState,CompileState,CompileWriter))
compileViaStr filepath config with from = do
  cs <- defaultCompileState
  rs <- defaultCompileReader config
  runCompile rs
             cs
             (parseResult (throwError . uncurry ParseError)
                          (fmap (\x -> execState (runPrinter (printJS x)) printConfig) . with)
                          (parseFay filepath from))

  where printConfig = def { psPretty = configPrettyPrint config }

-- | Compile a Haskell source string to a JavaScript source string.
compileToAst :: (Show from,Show to,CompilesTo from to)
              => FilePath
              -> CompileReader
              -> CompileState
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (to,CompileState,CompileWriter))
compileToAst filepath reader state with from =
  runCompile reader
             state
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

-- | Compile the top-level Fay module.
compileToplevelModule :: FilePath -> Module -> Compile [JsStmt]
compileToplevelModule filein mod@(Module _ (ModuleName modulename) _ _ _ _ _)  = do
  cfg <- config id
  when (configTypecheck cfg) $
    typecheck (configPackageConf cfg) (configWall cfg) $
      fromMaybe modulename $ configFilePath cfg
  initialPass mod
  cs <- io defaultCompileState
  modify $ \s -> s { stateImported = stateImported cs }
  fmap fst . listen $ compileModuleFromFile filein

--------------------------------------------------------------------------------
-- Compilers

-- | Read a file and compile.
compileModuleFromFile :: FilePath -> Compile [JsStmt]
compileModuleFromFile fp = io (readFile fp) >>= compileModule fp

-- | Compile a source string.
compileModuleFromContents :: String -> Compile [JsStmt]
compileModuleFromContents = compileModule "<interactive>"

-- | Lookup a module from include directories and compile.
compileModuleFromName :: ModuleName -> Compile [JsStmt]
compileModuleFromName name =
  unlessImported name compileModule
    where
      unlessImported :: ModuleName
                     -> (FilePath -> String -> Compile [JsStmt])
                     -> Compile [JsStmt]
      unlessImported "Fay.Types" _ = return []
      unlessImported name importIt = do
        imported <- gets stateImported
        case lookup name imported of
          Just _  -> return []
          Nothing -> do
            dirs <- configDirectoryIncludePaths <$> config id
            (filepath,contents) <- findImport dirs name
            modify $ \s -> s { stateImported = (name,filepath) : imported }
            importIt filepath contents

-- | Compile given the location and source string.
compileModule :: FilePath -> String -> Compile [JsStmt]
compileModule filepath contents = do
  state <- get
  reader <- ask
  result <- io $ compileToAst filepath reader state compileModuleFromAST contents
  case result of
    Right (stmts,state,writer) -> do
      modify $ \s -> s { stateImported      = stateImported state
                       , stateLocalScope    = S.empty
                       , stateJsModulePaths = stateJsModulePaths state
                       }
      maybeOptimize $ stmts ++ writerCons writer ++ makeTranscoding writer
    Left err -> throwError err
  where
    makeTranscoding :: CompileWriter -> [JsStmt]
    makeTranscoding CompileWriter{..} =
      let fay2js = if null writerFayToJs then [] else fayToJsHash writerFayToJs
          js2fay = if null writerJsToFay then [] else jsToFayHash writerJsToFay
      in fay2js ++ js2fay
    maybeOptimize :: [JsStmt] -> Compile [JsStmt]
    maybeOptimize stmts = do
      cfg <- config id
      return $ if configOptimize cfg
        then runOptimizer optimizeToplevel stmts
        else stmts

-- | Compile a parse HSE module.
compileModuleFromAST :: Module -> Compile [JsStmt]
compileModuleFromAST (Module _ modulename _pragmas Nothing _exports imports decls) =
  withModuleScope $ do
    imported <- fmap concat (mapM compileImport imports)
    modify $ \s -> s { stateModuleName = modulename
                     , stateModuleScope = fromMaybe (error $ "Could not find stateModuleScope for " ++ show modulename) $ M.lookup modulename $ stateModuleScopes s
                     }
    current <- compileDecls True decls

    exportStdlib     <- config configExportStdlib
    exportStdlibOnly <- config configExportStdlibOnly
    modulePaths      <- createModulePath modulename
    extExports       <- generateExports
    let stmts = imported ++ modulePaths ++ current ++ extExports
    return $ if exportStdlibOnly
      then if anStdlibModule modulename
              then stmts
              else []
      else if not exportStdlib && anStdlibModule modulename
              then []
              else stmts
compileModuleFromAST mod = throwError (UnsupportedModuleSyntax mod)

instance CompilesTo Module [JsStmt] where compileTo = compileModuleFromAST


-- | For a module A.B, generate
-- | var A = {};
-- | A.B = {};
createModulePath :: ModuleName -> Compile [JsStmt]
createModulePath =
  liftM concat . mapM modPath . mkModulePaths
  where
    modPath :: ModulePath -> Compile [JsStmt]
    modPath mp = whenImportNotGenerated mp $ \(unModulePath -> l) -> case l of
     [n] -> [JsVar (JsNameVar . UnQual $ Ident n) (JsObj [])]
     _   -> [JsSetModule mp (JsObj [])]

    whenImportNotGenerated :: ModulePath -> (ModulePath -> [JsStmt]) -> Compile [JsStmt]
    whenImportNotGenerated mp makePath = do
      added <- gets $ addedModulePath mp
      if added
        then return []
        else do
          modify $ addModulePath mp
          return $ makePath mp

-- | Generate exports for non local names, local exports have already been added to the module.
generateExports :: Compile [JsStmt]
generateExports = do
  m <- gets stateModuleName
  map (exportExp m) . S.toList . getNonLocalExports <$> gets id
  where
    exportExp :: ModuleName -> QName -> JsStmt
    exportExp m v = JsSetQName (changeModule m v) $ case findPrimOp v of
      Just p  -> JsName $ JsNameVar p
      Nothing -> JsName $ JsNameVar v

-- | Is the module a standard module, i.e., one that we'd rather not
-- output code for if we're compiling separate files.
anStdlibModule :: ModuleName -> Bool
anStdlibModule (ModuleName name) = name `elem` ["Prelude","FFI","Language.Fay.FFI","Data.Data"]

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
-- Package imports are ignored since they are used for some trickery in fay-base.
compileImport (ImportDecl _ _    _     _ Just{}  _       _) = return []
compileImport (ImportDecl _ name False _ Nothing Nothing _) = compileModuleFromName name
compileImport i = throwError $ UnsupportedImport i
