{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | The Haskell→Javascript compiler.

module Fay.Compiler
  (runCompileModule
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
import           Fay.Compiler.InitialPass        (initialPass)
import           Fay.Compiler.Misc
import           Fay.Compiler.Optimizer
import           Fay.Compiler.PrimOp             (findPrimOp)
import           Fay.Compiler.QName
import           Fay.Compiler.State
import           Fay.Compiler.Typecheck
import           Fay.Control.Monad.IO
import qualified Fay.Exts                        as F
import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.NoAnnotation           as N
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Default                    (def)
import           Data.Maybe
import qualified Data.Set                        as S
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names

--------------------------------------------------------------------------------
-- Top level entry points

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: FilePath
              -> CompileConfig
              -> (F.Module -> Compile [JsStmt])
              -> String
              -> IO (Either CompileError (PrintState,CompileState,CompileWriter))
compileViaStr filepath config with from = do
  rs <- defaultCompileReader config
  runTopCompile rs
             defaultCompileState
             (parseResult (throwError . uncurry ParseError)
                          (fmap (\x -> execState (runPrinter (printJS x)) printConfig) . with)
                          (parseFay filepath from))

  where printConfig = def { psPretty = configPrettyPrint config }

-- | Compile a Haskell source string to a JavaScript source string.
compileToAst :: FilePath
              -> CompileReader
              -> CompileState
              -> (F.Module -> Compile [JsStmt])
              -> String
              -> Compile (CompileResult [JsStmt])
compileToAst filepath reader state with from =
  Compile . lift . lift $ runCompileModule reader
             state
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

-- | Compile the top-level Fay module.
compileToplevelModule :: FilePath -> F.Module -> Compile [JsStmt]
compileToplevelModule filein mod@Module{}  = do
  cfg <- config id
  when (configTypecheck cfg) $
    typecheck (configPackageConf cfg) (configWall cfg) $
      fromMaybe (F.moduleNameString (F.moduleName mod)) $ configFilePath cfg
  initialPass mod
  -- Reset imports after initialPass so the modules can be imported during code generation.
  modify $ \s -> s { stateImported = [] }
  fmap fst . listen $ compileModuleFromFile filein
compileToplevelModule _ m = throwError $ UnsupportedModuleSyntax "compileToplevelModule" m

--------------------------------------------------------------------------------
-- Compilers

-- | Read a file and compile.
compileModuleFromFile :: FilePath -> Compile [JsStmt]
compileModuleFromFile fp = io (readFile fp) >>= compileModule fp

-- | Compile a source string.
compileModuleFromContents :: String -> Compile [JsStmt]
compileModuleFromContents = compileModule "<interactive>"

-- | Lookup a module from include directories and compile.
compileModuleFromName :: F.ModuleName -> Compile [JsStmt]
compileModuleFromName name =
  unlessImported name compileModule
    where
      unlessImported :: ModuleName a
                     -> (FilePath -> String -> Compile [JsStmt])
                     -> Compile [JsStmt]
      unlessImported (ModuleName _ "Fay.Types") _ = return []
      unlessImported (unAnn -> name) importIt = do
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
  result <- compileToAst filepath reader state compileModuleFromAST contents
  case result of
    Right (stmts,state,writer) -> do
      modify $ \s -> s { stateImported      = stateImported state
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
compileModuleFromAST :: F.Module -> Compile [JsStmt]
compileModuleFromAST mod'@(Module _ _ pragmas imports _) = do
  mod@(Module _ _ _ _ decls) <- annotateModule Haskell2010 [] mod'
  let modName = unAnn $ F.moduleName mod
  imported <- fmap concat (mapM compileImport imports)
  modify $ \s -> s { stateModuleName = modName
                   , stateUseFromString = hasLanguagePragmas ["OverloadedStrings", "RebindableSyntax"] pragmas
                   }
  current <- compileDecls True decls

  exportStdlib     <- config configExportStdlib
  exportStdlibOnly <- config configExportStdlibOnly
  modulePaths      <- createModulePath modName
  extExports       <- generateExports
  let stmts = imported ++ modulePaths ++ current ++ extExports
  return $ if exportStdlibOnly
    then if anStdlibModule modName
            then stmts
            else []
    else if not exportStdlib && anStdlibModule modName
            then []
            else stmts
compileModuleFromAST mod = throwError (UnsupportedModuleSyntax "compileModuleFromAST" mod)

hasLanguagePragmas :: [String] -> [F.ModulePragma] -> Bool
hasLanguagePragmas pragmas modulePragmas = (== length pragmas) . length . filter (`elem` pragmas) $ flattenPragmas modulePragmas
  where
    flattenPragmas :: [F.ModulePragma] -> [String]
    flattenPragmas ps = concat $ map pragmaName ps
    pragmaName (LanguagePragma _ q) = map unname q
    pragmaName _ = []

-- | For a module A.B, generate
-- | var A = {};
-- | A.B = {};
createModulePath :: ModuleName a -> Compile [JsStmt]
createModulePath (unAnn -> m) =
  liftM concat . mapM modPath . mkModulePaths $ m
  where
    modPath :: ModulePath -> Compile [JsStmt]
    modPath mp = whenImportNotGenerated mp $ \(unModulePath -> l) -> case l of
     [n] -> [JsVar (JsNameVar . UnQual () $ Ident () n) (JsObj [])]
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
  modName <- gets stateModuleName
  maybe [] (map (exportExp modName) . S.toList) <$> gets (getNonLocalExportsWithoutNewtypes modName)
  where
    exportExp :: N.ModuleName -> N.QName -> JsStmt
    exportExp m v = JsSetQName (changeModule m v) $ case findPrimOp v of
      Just p  -> JsName $ JsNameVar p -- TODO add test case for this case, is it needed at all?
      Nothing -> JsName $ JsNameVar v

-- | Is the module a standard module, i.e., one that we'd rather not
-- output code for if we're compiling separate files.
anStdlibModule :: ModuleName a -> Bool
anStdlibModule (ModuleName _ name) = name `elem` ["Prelude","FFI","Fay.FFI","Data.Data"]

-- | Compile the given import.
compileImport :: F.ImportDecl -> Compile [JsStmt]
-- Package imports are ignored since they are used for some trickery in fay-base.
compileImport (ImportDecl _ _    _     _ Just{}  _       _) = return []
compileImport (ImportDecl _ name False _ Nothing Nothing _) = compileModuleFromName name
compileImport i = throwError $ UnsupportedImport i
