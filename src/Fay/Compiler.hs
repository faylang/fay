{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

-- | The Haskell→Javascript compiler.

module Fay.Compiler
  (runCompileModule
  ,compileViaStr
  ,compileWith
  ,compileExp
  ,compileDecl
  ,compileToplevelModule
  ,compileModuleFromContents
  ,compileModuleFromAST
  ,parseFay)
  where

import           Fay.Compiler.Prelude

import           Fay.Compiler.Decl
import           Fay.Compiler.Defaults
import           Fay.Compiler.Desugar
import           Fay.Compiler.Exp
import           Fay.Compiler.FFI
import           Fay.Compiler.Import
import           Fay.Compiler.InitialPass        (initialPass)
import           Fay.Compiler.Misc
import           Fay.Compiler.Optimizer
import           Fay.Compiler.Parse
import           Fay.Compiler.PrimOp             (findPrimOp)
import           Fay.Compiler.QName
import           Fay.Compiler.State
import           Fay.Compiler.Typecheck
import           Fay.Config
import qualified Fay.Exts                        as F
import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.NoAnnotation           as N
import           Fay.Types

import           Control.Monad.Except            (throwError)
import           Control.Monad.RWS               (gets, modify)

import qualified Data.Set                        as S
import           Language.Haskell.Exts hiding (name)
import           Language.Haskell.Names          (annotateModule)

--------------------------------------------------------------------------------
-- Top level entry points

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr

  :: FilePath
  -> Config
  -> (F.Module -> Compile [JsStmt])
  -> String
  -> IO (Either CompileError (Printer,CompileState,CompileWriter))
compileViaStr filepath cfg with from = do
  rs <- defaultCompileReader cfg
  runTopCompile rs
             defaultCompileState
             (parseResult (throwError . uncurry ParseError)
                          (fmap (mconcat . map printJS) . with)
                          (parseFay filepath from))

-- | Compile the top-level Fay module.
compileToplevelModule :: FilePath -> F.Module -> Compile [JsStmt]
compileToplevelModule filein mod@Module{}  = do
  cfg <- config id
  when (configTypecheck cfg) $ do
    res <- io $ typecheck cfg $
             fromMaybe (F.moduleNameString (F.moduleName mod)) $
               configFilePath cfg
    either throwError warn res
  initialPass filein
  -- Reset imports after initialPass so the modules can be imported during code generation.
  (hstmts, fstmts) <- startCompile compileFileWithSource filein
  return (hstmts++fstmts)
compileToplevelModule _ m = throwError $ UnsupportedModuleSyntax "compileToplevelModule" m

--------------------------------------------------------------------------------
-- Compilers

-- | Compile a source string.
compileModuleFromContents :: String -> Compile ([JsStmt], [JsStmt])
compileModuleFromContents = compileFileWithSource "<interactive>"

-- | Compile given the location and source string.
compileFileWithSource :: FilePath -> String -> Compile ([JsStmt], [JsStmt])
compileFileWithSource filepath contents = do
  exportStdlib <- config configExportStdlib
  ((hstmts,fstmts),st,wr) <- compileWith filepath compileModuleFromAST compileFileWithSource desugar contents
  modify $ \s -> s { stateImported      = stateImported      st
                   , stateJsModulePaths = stateJsModulePaths st
                   }
  hstmts' <- maybeOptimize $ hstmts ++ writerCons wr ++ makeTranscoding exportStdlib (stateModuleName st) wr
  fstmts' <- maybeOptimize fstmts
  return (hstmts', fstmts')
  where
    makeTranscoding :: Bool -> ModuleName a -> CompileWriter -> [JsStmt]
    makeTranscoding exportStdlib moduleName CompileWriter{..} =
      let fay2js = if null writerFayToJs || (anStdlibModule moduleName && not exportStdlib)
                     then []
                     else fayToJsHash writerFayToJs
          js2fay = if null writerJsToFay || (anStdlibModule moduleName && not exportStdlib)
                     then []
                     else jsToFayHash writerJsToFay
      in fay2js ++ js2fay
    maybeOptimize :: [JsStmt] -> Compile [JsStmt]
    maybeOptimize stmts = do
      cfg <- config id
      return $ if configOptimize cfg
        then runOptimizer optimizeToplevel stmts
        else stmts

-- | Compile a parse HSE module.
compileModuleFromAST :: ([JsStmt], [JsStmt]) -> F.Module -> Compile ([JsStmt], [JsStmt])
compileModuleFromAST (hstmts0, fstmts0) mod'@Module{} = do
  mod@(Module _ _ pragmas _ decls) <- annotateModule Haskell2010 defaultExtensions mod'
  let modName = unAnn $ F.moduleName mod
  modify $ \s -> s { stateUseFromString = hasLanguagePragmas ["OverloadedStrings", "RebindableSyntax"] pragmas
                   }
  current <- compileDecls True decls

  exportStdlib     <- config configExportStdlib
  exportStdlibOnly <- config configExportStdlibOnly
  modulePaths      <- createModulePath modName
  extExports       <- generateExports
  strictExports    <- generateStrictExports
  let hstmts = hstmts0 ++ modulePaths ++ current ++ extExports
      fstmts = fstmts0 ++ strictExports
  return $ if exportStdlibOnly
    then if anStdlibModule modName
            then (hstmts, fstmts)
            else ([], [])
    else if not exportStdlib && anStdlibModule modName
            then ([], [])
            else (hstmts, fstmts)
compileModuleFromAST _ mod = throwError $ UnsupportedModuleSyntax "compileModuleFromAST" mod


--------------------------------------------------------------------------------
-- Misc compilation

-- | For a module A.B, generate
-- | var A = {};
-- | A.B = {};
createModulePath :: ModuleName a -> Compile [JsStmt]
createModulePath (unAnn -> m) = do
  cfg <- config id
  reg <- liftM concat . mapM modPath . mkModulePaths $ m
  strict <-
    if shouldExportStrictWrapper m cfg
      then liftM concat . mapM modPath . mkModulePaths $ (\(ModuleName i n) -> ModuleName i ("Strict." ++ n)) m
       else return []
  return $ reg ++ strict
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
    exportExp m v = JsSetQName Nothing (changeModule m v) $ case findPrimOp v of
      Just p  -> JsName $ JsNameVar p -- TODO add test case for this case, is it needed at all?
      Nothing -> JsName $ JsNameVar v

-- | Generate strict wrappers for the exports of the module.
generateStrictExports :: Compile [JsStmt]
generateStrictExports = do
  cfg <- config id
  modName <- gets stateModuleName
  if shouldExportStrictWrapper modName cfg
    then do
      locals <- gets (getLocalExportsWithoutNewtypes modName)
      nonLocals <- gets (getNonLocalExportsWithoutNewtypes modName)
      let int = maybe [] (map exportExp' . S.toList) locals
      let ext = maybe [] (map (exportExp modName)  . S.toList) nonLocals
      return $ int ++ ext
    else return []
  where
    exportExp :: N.ModuleName -> N.QName -> JsStmt
    exportExp m v = JsSetQName Nothing (changeModule' ("Strict." ++) $ changeModule m v) $ JsName $ JsNameVar $ changeModule' ("Strict." ++) v

    exportExp' :: N.QName -> JsStmt
    exportExp' name = JsSetQName Nothing (changeModule' ("Strict." ++) name) $ serialize (JsName (JsNameVar name))

    serialize :: JsExp -> JsExp
    serialize n = JsApp (JsRawExp "Fay$$fayToJs") [JsRawExp "['automatic']", n]

-- | Is the module a standard module, i.e., one that we'd rather not
-- output code for if we're compiling separate files.
anStdlibModule :: ModuleName a -> Bool
anStdlibModule (ModuleName _ name) = name `elem` ["Prelude","FFI","Fay.FFI","Data.Data","Data.Ratio","Debug.Trace","Data.Char"]
