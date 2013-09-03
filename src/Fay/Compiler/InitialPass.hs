{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Preprocessing collecting names, data types, newtypes, imports, and exports
-- for all modules recursively.
module Fay.Compiler.InitialPass
  (initialPass
  ) where

import           Fay.Compiler.Config
import           Fay.Compiler.GADT
import           Fay.Compiler.Misc
import           Fay.Data.List.Extra
import qualified Fay.Exts                        as F
import           Fay.Exts.NoAnnotation           (unAnn)
import qualified Fay.Exts.NoAnnotation           as N
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.RWS
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Language.Haskell.Exts.Annotated hiding (name, var)
import qualified Language.Haskell.Names          as HN
import           Prelude                         hiding (mod, read)

-- | Preprocess and collect all information needed during code generation.
initialPass :: F.Module -> Compile ()
initialPass mod@(Module _ _ _pragmas imports decls) = do
  modify $ \s -> s { stateModuleName = unAnn (F.moduleName mod)
                   }
  forM_ imports compileImport
  -- This can only return one element since we only compile one module.
  ([exports],_) <- HN.getInterfaces Haskell2010 [] [mod]
  modify $ \s -> s { stateInterfaces = M.insert (stateModuleName s) exports $ stateInterfaces s }
  forM_ decls scanRecordDecls
  forM_ decls scanNewtypeDecls
initialPass m = throwError (UnsupportedModuleSyntax "initialPass" m)

compileImport :: F.ImportDecl -> Compile ()
compileImport (ImportDecl _ _ _ _ Just{} _ _) = return ()
compileImport (ImportDecl _ name False _ Nothing Nothing _) =
  compileImport' name
compileImport i =
  throwError $ UnsupportedImport i

compileWith :: (Show from,Parseable from)
            => FilePath
            -> CompileReader
            -> CompileState
            -> (from -> Compile ())
            -> String
            -> Compile (AllTheState ())
compileWith filepath r st with from =
  Compile . lift . lift $ runCompile r
             st
             (parseResult (throwError . uncurry ParseError)
             with
             (parseFay filepath from))

-- | Don't re-import the same modules.
unlessImported :: ModuleName a
               -> (FilePath -> String -> Compile ())
               -> Compile ()
unlessImported name importIt = do
  isImported <- lookup (unAnn name) <$> gets stateImported
  case isImported of
    Just _ -> return ()
    Nothing -> do
      dirs <- configDirectoryIncludePaths <$> config id
      (filepath,contents) <- findImport dirs (unAnn name)
      modify $ \s -> s { stateImported = ((unAnn name),filepath) : stateImported s }
      importIt filepath contents

-- | Find newtype declarations
scanNewtypeDecls :: F.Decl -> Compile ()
scanNewtypeDecls (DataDecl _ NewType{} _ _ constructors _) = compileNewtypeDecl constructors
scanNewtypeDecls _ = return ()

-- | Add new types to the state
compileNewtypeDecl :: [F.QualConDecl] -> Compile ()
compileNewtypeDecl [QualConDecl _ _ _ condecl] =
  case condecl of
      -- newtype declaration without destructor
    ConDecl _ name  [ty]            -> addNewtype name Nothing ty
    RecDecl _ cname [FieldDecl _ [dname] ty] -> addNewtype cname (Just dname) ty
    x -> error $ "compileNewtypeDecl case: Should be impossible (this is a bug). Got: " ++ show x
  where
    getBangTy :: F.BangType -> N.Type
    getBangTy (BangedTy _ t)   = unAnn t
    getBangTy (UnBangedTy _ t) = unAnn t
    getBangTy (UnpackedTy _ t) = unAnn t

    addNewtype cname dname ty = do
      qcname <- qualify cname
      qdname <- case dname of
                  Nothing -> return Nothing
                  Just n  -> Just <$> qualify n
      modify (\cs@CompileState{stateNewtypes=nts} ->
               cs{stateNewtypes=(qcname,qdname,getBangTy ty):nts})
compileNewtypeDecl q = error $ "compileNewtypeDecl: Should be impossible (this is a bug). Got: " ++ show q

declHeadName :: F.DeclHead -> F.Name
declHeadName d = case d of
  DHead _ n _ -> n
  DHInfix _ _ n _ -> n
  DHParen _ h -> declHeadName h

-- | Add record declarations to the state
scanRecordDecls :: F.Decl -> Compile ()
scanRecordDecls decl = do
  case decl of
    DataDecl _loc DataType{} _ctx (declHeadName -> name) qualcondecls _deriv -> do
      let ns = for qualcondecls (\(QualConDecl _loc' _tyvarbinds _ctx' condecl) -> conDeclName condecl)
      addRecordTypeState name ns
    _ -> return ()

  case decl of
    DataDecl _ DataType{} _ _ constructors _ -> dataDecl constructors
    GDataDecl _ DataType{} _ _ _ decls _ -> dataDecl (map convertGADT decls)
    _ -> return ()

  where
    addRecordTypeState (unAnn -> name) (map unAnn -> cons) = modify $ \s -> s
      { stateRecordTypes = (UnQual () name, map (UnQual ()) cons) : stateRecordTypes s }

    conDeclName (ConDecl _ n _) = n
    conDeclName (InfixConDecl _ _ n _) = n
    conDeclName (RecDecl _ n _) = n

    -- | Collect record definitions and store record name and field names.
    -- A ConDecl will have fields named slot1..slotN
    dataDecl :: [F.QualConDecl] -> Compile ()
    dataDecl constructors = do
      forM_ constructors $ \(QualConDecl _ _ _ condecl) ->
        case condecl of
          ConDecl _ name types -> do
            let fields =  map (Ident () . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
            addRecordState name fields
          InfixConDecl _ _t1 name _t2 ->
            addRecordState name [F.mkIdent "slot1", F.mkIdent "slot2"]
          RecDecl _ name fields' -> do
            let fields = concatMap F.fieldDeclNames fields'
            addRecordState name fields

      where
        addRecordState :: Name a -> [Name b] -> Compile ()
        addRecordState (unAnn -> name) (map unAnn -> fields) = modify $ \s -> s
          { stateRecords = (UnQual () name,map (UnQual ()) fields) : stateRecords s }

-- | Compile an import filtering the exports based on the current module's imports
compileImport' :: F.ModuleName -> Compile ()
compileImport' name =
  unlessImported name $ \filepath contents -> do
    read <- ask
    stat <- get
    result <- compileWith filepath read stat initialPass contents
    case result of
      Right ((),st,_) -> do
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords      = stateRecords st
                         , stateLocalScope   = S.empty
                         , stateRecordTypes  = stateRecordTypes st
                         , stateImported     = stateImported st
                         , stateNewtypes     = stateNewtypes st
                         , stateInterfaces   = stateInterfaces st
                         }
      Left err -> throwError err
