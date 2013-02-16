{-# LANGUAGE OverloadedStrings #-}

module Fay.Compiler.InitialPass where

import Fay.Compiler.Misc
import Fay.Types
import Fay.Compiler.Config
import Fay.Compiler.Decl (compileNewtypeDecl)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser

initialPass :: Module -> Compile ()
initialPass (Module _ _ _ Nothing _ imports decls) = do
  forM_ imports $ \imp ->
    case imp of
      ImportDecl _ _ _ _ Just{} _ _ -> return ()

      ImportDecl _ name False _ Nothing Nothing _ ->
        void $ unlessImported name $ \filepath contents -> do
          result <- compileWith filepath initialPass contents
          case result of
            Right ((),st,_) ->
              -- Merges the state gotten from passing through an imported
              -- module with the current state. We can assume no duplicate
              -- records exist since GHC would pick that up.
              modify $ \s -> s { stateRecords = stateRecords st
                               , stateRecordTypes = stateRecordTypes st
                               , stateImported = stateImported st
                               , stateNewtypes = stateNewtypes st
                               }
            Left err -> throwError err

      i -> throwError $ UnsupportedImport i

  forM_ decls scanRecordDecls
  forM_ decls scanNewtypeDecls

initialPass m = throwError (UnsupportedModuleSyntax m)

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
