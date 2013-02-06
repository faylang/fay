{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | The Haskell→Javascript compiler.

module Fay.Compiler
  (runCompile
  ,compileViaStr
  ,compileForDocs
  ,compileToAst
  ,compileModule
  ,compileExp
  ,compileDecl
  ,compileToplevelModule
  ,parseFay)
  where

import           Fay.Compiler.CollectRecords (collectRecords)
import           Fay.Compiler.Config
import           Fay.Compiler.Defaults
import           Fay.Compiler.Exp
import           Fay.Compiler.Decl
import           Fay.Compiler.FFI
import           Fay.Compiler.Misc
import           Fay.Compiler.ModuleScope (bindAsLocals, findTopLevelNames, moduleLocals)
import           Fay.Compiler.Optimizer
import           Fay.Compiler.Typecheck
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Control.Monad.RWS
import           Data.Default                    (def)
import qualified Data.Set                        as S
import           Data.Maybe
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
             (cs { stateFilePath = filepath })
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

-- | Compile the given Fay code for the documentation. This is
-- specialised because the documentation isn't really “real”
-- compilation.
compileForDocs :: Module -> Compile [JsStmt]
compileForDocs mod = do
  collectRecords mod
  compileModule False mod

-- | Compile the top-level Fay module.
compileToplevelModule :: Module -> Compile [JsStmt]
compileToplevelModule mod@(Module _ (ModuleName modulename) _ _ _ _ _)  = do
  cfg <- config id

  when (configTypecheck cfg) $
    typecheck (configPackageConf cfg) (configWall cfg) $
      fromMaybe modulename $ configFilePath cfg
  collectRecords mod
  cs <- io defaultCompileState
  modify $ \s -> s { stateImported = stateImported cs }
  (stmts,CompileWriter{..}) <- listen $ compileModule True mod
  let fay2js = if null writerFayToJs then [] else [fayToJsDispatcher writerFayToJs]
      js2fay = if null writerJsToFay then [] else [jsToFayDispatcher writerJsToFay]
      maybeOptimize = if configOptimize cfg then runOptimizer optimizeToplevel else id
  if configDispatcherOnly cfg
     then return (maybeOptimize (writerCons ++ fay2js ++ js2fay))
     else return (maybeOptimize (stmts ++
                    if configDispatchers cfg then writerCons ++ fay2js ++ js2fay else []))

--------------------------------------------------------------------------------
-- Compilers

-- | Compile Haskell module.
compileModule :: Bool -> Module -> Compile [JsStmt]
compileModule toplevel (Module _ modulename _pragmas Nothing exports imports decls) =
  withModuleScope $ do
    modify $ \s -> s { stateModuleName = modulename
                     , stateModuleScope = findTopLevelNames modulename decls
                     }
    imported <- fmap concat (mapM compileImport imports)
    current <- compileDecls True decls

    case exports of
      Just exps -> mapM_ emitExport exps
      Nothing -> do
        exps <- moduleLocals modulename <$> gets stateModuleScope
        modify $ flip (foldr addCurrentExport) exps

    exportStdlib     <- config configExportStdlib
    exportStdlibOnly <- config configExportStdlibOnly
    if exportStdlibOnly
       then if anStdlibModule modulename || toplevel
               then if toplevel
                       then return imported
                       else return (current ++ imported)
               else return []
       else if not exportStdlib && anStdlibModule modulename
               then return []
               else return (imported ++ current)
compileModule _ mod = throwError (UnsupportedModuleSyntax mod)

instance CompilesTo Module [JsStmt] where compileTo = compileModule False

-- | Is the module a standard module, i.e., one that we'd rather not
-- output code for if we're compiling separate files.
anStdlibModule :: ModuleName -> Bool
anStdlibModule (ModuleName name) = elem name ["Prelude","FFI","Language.Fay.FFI","Data.Data"]

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ _ _ _ Just{} _ _) = do
--  warn $ "import with package syntax ignored: " ++ prettyPrint i
  return []
compileImport (ImportDecl _ name False _ Nothing Nothing Nothing) =
  compileImportWithFilter name (const $ return True)
compileImport (ImportDecl _ name False _ Nothing Nothing (Just (True, specs))) =
  compileImportWithFilter name (fmap not . imported specs)
compileImport (ImportDecl _ name False _ Nothing Nothing (Just (False, specs))) =
  compileImportWithFilter name (imported specs)
compileImport i =
  throwError $ UnsupportedImport i

imported :: [ImportSpec] -> QName -> Compile Bool
imported is qn = anyM (matching qn) is
  where
    matching :: QName -> ImportSpec -> Compile Bool
    matching (Qual _ _) (IAbs _) = return True -- Types are always OK
    matching (Qual _ name) (IVar var) = return $ name == var
    matching (Qual _ name) (IThingAll typ) = do
      recs <- typeToRecs $ UnQual typ
      if UnQual name `elem` recs
        then return True
        else do
          fields <- typeToFields $ UnQual typ
          return $ UnQual name `elem` fields
    matching (Qual _ name) (IThingWith typ cns) =
      flip anyM cns $ \cn -> case cn of
        ConName _ -> do
          recs <- typeToRecs $ UnQual typ
          return $ UnQual name `elem` recs
        VarName _ -> do
          fields <- typeToFields $ UnQual typ
          return $ UnQual name `elem` fields
    matching q is = error $ "compileImport: Unsupported QName ImportSpec combination " ++ show (q, is) ++ ", this is a bug!"


compileImportWithFilter :: ModuleName -> (QName -> Compile Bool) -> Compile [JsStmt]
compileImportWithFilter name importFilter =
  unlessImported name importFilter $ \filepath contents -> do
    state <- get
    reader <- ask
    result <- liftIO $ compileToAst filepath reader state (compileModule False) contents
    case result of
      Right (stmts,state,writer) -> do
        imports <- filterM importFilter $ S.toList $ getCurrentExports state
        tell writer
        modify $ \s -> s { stateImported    = stateImported state
                         , stateLocalScope  = S.empty
                         , stateModuleScope = bindAsLocals imports (stateModuleScope s)
                         , _stateExports    = _stateExports state
                         }
        return stmts
      Left err -> throwError err

unlessImported :: ModuleName
               -> (QName -> Compile Bool)
               -> (FilePath -> String -> Compile [JsStmt])
               -> Compile [JsStmt]
unlessImported "Fay.Types" _ _ = return []
unlessImported name importFilter importIt = do
  imported <- gets stateImported
  case lookup name imported of
    Just _ -> do
      exports <- gets $ getExportsFor name
      imports <- filterM importFilter $ S.toList exports
      modify $ \s -> s { stateModuleScope = bindAsLocals imports (stateModuleScope s) }
      return []
    Nothing -> do
      dirs <- configDirectoryIncludePaths <$> config id
      (filepath,contents) <- findImport dirs name
      res <- importIt filepath contents
                         -- TODO stateImported is already added in initialPass so it is not needed here
                         -- but one Api test fails if it's removed.
      modify $ \s -> s { stateImported     = (name,filepath) : imported
                       }
      return res
