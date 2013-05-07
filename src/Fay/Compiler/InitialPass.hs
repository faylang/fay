{-# LANGUAGE OverloadedStrings #-}

module Fay.Compiler.InitialPass where

import Fay.Compiler.Misc
import Fay.Types
import Fay.Compiler.Config
import Fay.Compiler.Decl (compileNewtypeDecl)
import Fay.Compiler.GADT

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser

import qualified Data.Set as S
import Fay.Compiler.ModuleScope
import Control.Monad.Extra

initialPass :: Module -> Compile ()
initialPass (Module _ _ _ Nothing _ imports decls) = do
  forM_ imports compileImport
  forM_ decls scanRecordDecls
  forM_ decls scanNewtypeDecls

initialPass m = throwError (UnsupportedModuleSyntax m)

compileImport :: ImportDecl -> Compile ()
compileImport (ImportDecl _ _ _ _ Just{} _ _) = return ()
compileImport (ImportDecl _ name False _ Nothing Nothing Nothing) =
  compileImportWithFilter name (const $ return True)
compileImport (ImportDecl _ name False _ Nothing Nothing (Just (True, specs))) =
  compileImportWithFilter name (fmap not . imported specs)
compileImport (ImportDecl _ name False _ Nothing Nothing (Just (False, specs))) =
  compileImportWithFilter name (imported specs)
compileImport i =
  throwError $ UnsupportedImport i

compileWith :: (Show from,Parseable from)
            => FilePath
            -> (from -> Compile ())
            -> String
            -> Compile (Either CompileError ((),CompileState,CompileWriter))
compileWith filepath with from = do
  compileReader <- ask
  compileState  <- get
  liftIO $ runCompile compileReader
                      compileState
                      (parseResult (throwError . uncurry ParseError)
                                   with
                                   (parseFay filepath from))

-- | Don't re-import the same modules.
unlessImported :: ModuleName
               -> (QName -> Compile Bool)
               -> (FilePath -> String -> Compile ())
               -> Compile ()
unlessImported "Fay.Types" _ _ = return ()
unlessImported name importFilter importIt = do
  imported <- gets stateImported
  case lookup name imported of
    Just _ -> do
      exports <- gets $ getExportsFor name
      imports <- filterM importFilter $ S.toList exports
      modify $ \s -> s { stateModuleScope = bindAsLocals imports (stateModuleScope s) }
    Nothing -> do
      dirs <- configDirectoryIncludePaths <$> config id
      (filepath,contents) <- findImport dirs name
      modify $ \s -> s { stateImported = (name,filepath) : imported }
      importIt filepath contents

scanNewtypeDecls :: Decl -> Compile ()
scanNewtypeDecls (DataDecl _ NewType _ _ _ constructors _) =
  void $ compileNewtypeDecl constructors
scanNewtypeDecls _ = return ()

scanRecordDecls :: Decl -> Compile ()
scanRecordDecls decl = do
  case decl of
    DataDecl _loc DataType _ctx name _tyvarb qualcondecls _deriv -> do
      let ns = flip map qualcondecls (\(QualConDecl _loc' _tyvarbinds _ctx' condecl) -> conDeclName condecl)
      addRecordTypeState name ns
    _ -> return ()


  case decl of
    DataDecl _ DataType _ _ _ constructors _ -> dataDecl constructors
    GDataDecl _ DataType _l _i _v _n decls _ -> dataDecl (map convertGADT decls)
    _ -> return ()

  where
    addRecordTypeState name cons = modify $ \s -> s
      { stateRecordTypes = (UnQual name, map UnQual cons) : stateRecordTypes s }

    conDeclName (ConDecl n _) = n
    conDeclName (InfixConDecl _ n _) = n
    conDeclName (RecDecl n _) = n

    -- | Collect record definitions and store record name and field names.
    -- A ConDecl will have fields named slot1..slotN
    dataDecl :: [QualConDecl] -> Compile ()
    dataDecl constructors = do
      forM_ constructors $ \(QualConDecl _ _ _ condecl) ->
        case condecl of
          ConDecl name types -> do
            let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
            addRecordState name fields
          InfixConDecl _t1 name _t2 ->
            addRecordState name ["slot1", "slot2"]
          RecDecl name fields' -> do
            let fields = concatMap fst fields'
            addRecordState name fields

      where
        addRecordState :: Name -> [Name] -> Compile ()
        addRecordState name fields = modify $ \s -> s
          { stateRecords = (UnQual name,map UnQual fields) : stateRecords s }

------------------------------------

imported :: [ImportSpec] -> QName -> Compile Bool
imported is qn = anyM (matching qn) is
  where
    matching :: QName -> ImportSpec -> Compile Bool
    matching (Qual _ name) (IAbs typ) = return (name == typ)
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


compileImportWithFilter :: ModuleName -> (QName -> Compile Bool) -> Compile ()
compileImportWithFilter name importFilter =
  unlessImported name importFilter $ \filepath contents -> do
    result <- compileWith filepath initialPass contents
    case result of
      Right ((),st,_) -> do
        imports <- filterM importFilter $ S.toList $ getCurrentExports st
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords = stateRecords st
                         , stateLocalScope  = S.empty
                         , stateRecordTypes = stateRecordTypes st
                         , stateImported = stateImported st
                         , stateNewtypes = stateNewtypes st
                         , stateModuleScope = bindAsLocals imports (stateModuleScope s)
                         , _stateExports = _stateExports st
                         }
      Left err -> throwError err
