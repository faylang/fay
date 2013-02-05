{-# LANGUAGE OverloadedStrings #-}

module Language.Fay.Compiler.CollectRecords where

import Language.Fay.Compiler.Misc
import Language.Fay.Types

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Language.Haskell.Exts

--------------------------------------------------------------------------------
-- Initial pass-through collecting record definitions

collectRecords :: Module -> Compile ()
collectRecords (Module _ _ _ Nothing _ imports decls) = do
  mapM_ passImport imports
  mapM_ decl decls
collectRecords mod = throwError (UnsupportedModuleSyntax mod)

passImport :: ImportDecl -> Compile ()
passImport (ImportDecl _ _ _ _ Just{} _ _) = do
  return ()
--  warn $ "import with package syntax ignored: " ++ prettyPrint i
passImport (ImportDecl _ name False _ Nothing Nothing _) = do
  void $ unlessImported name $ \filepath contents -> do
    state <- get
    reader <- ask
    result <- liftIO $ records filepath reader state collectRecords contents
    case result of
      Right ((),st) -> do
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords = stateRecords st
                         , stateRecordTypes = stateRecordTypes st
                         , stateImported = stateImported st
                         }
      Left err -> throwError err
    return ()
passImport i = throwError $ UnsupportedImport i

-- | Don't re-import the same modules.
unlessImported :: ModuleName
                           -> (FilePath -> String -> Compile ())
                           -> Compile ()
unlessImported name importIt = do
  imported <- gets stateImported
  case lookup name imported of
    Just _ -> return ()
    Nothing -> do
      dirs <- configDirectoryIncludePaths <$> config id
      (filepath,contents) <- findImport dirs name
      modify $ \s -> s { stateImported = (name,filepath) : imported }
      importIt filepath contents

records :: (Show from,Parseable from)
                    => FilePath
                    -> CompileReader
                    -> CompileState
                    -> (from -> Compile ())
                    -> String
                    -> IO (Either CompileError ((),CompileState))
records filepath compileReader compileState with from =
  runCompile compileReader
             compileState
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

decl :: Decl -> Compile ()
decl decl = do
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
