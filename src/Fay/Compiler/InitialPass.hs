{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Preprocessing collecting names, data types, newtypes, imports, and exports
-- for all modules recursively.
module Fay.Compiler.InitialPass
  (initialPass
  ) where

import           Fay.Compiler.Prelude

import           Fay.Compiler.Desugar
import           Fay.Compiler.GADT
import           Fay.Compiler.Import
import           Fay.Compiler.Misc
import           Fay.Compiler.Parse
import qualified Fay.Exts                        as F
import           Fay.Exts.NoAnnotation           (unAnn)
import           Fay.Types

import           Control.Monad.Error
import           Control.Monad.RWS
import qualified Data.Map                        as M
import           Language.Haskell.Exts.Annotated hiding (name, var)
import qualified Language.Haskell.Names          as HN

-- | Preprocess and collect all information needed during code generation.
initialPass :: FilePath -> Compile ()
initialPass = startCompile preprocessFileWithSource

-- | Preprocess a module given its filepath and content.
preprocessFileWithSource :: FilePath -> String -> Compile ()
preprocessFileWithSource filepath contents = do
  (_,st,_) <- compileWith filepath preprocessAST preprocessFileWithSource desugar contents
  -- This is the state we want to keep
  modify $ \s -> s { stateRecords     = stateRecords     st
                   , stateRecordTypes = stateRecordTypes st
                   , stateImported    = stateImported    st
                   , stateNewtypes    = stateNewtypes    st
                   , stateInterfaces  = stateInterfaces  st
                   , stateTypeSigs    = stateTypeSigs    st
                     -- TODO This needs to be added otherwise the
                     -- "executable" generation in Fay.hs gets the
                     -- wrong name. Not sure why it works to do it
                     -- here!
                   , stateModuleName  = stateModuleName  st
                   }

-- | Preprocess from an AST
preprocessAST :: () -> F.Module -> Compile ()
preprocessAST () mod@(Module _ _ _ _ decls) = do
  -- This can only return one element since we only compile one module.
  ([exports],_) <- HN.getInterfaces Haskell2010 defaultExtensions [mod]
  modify $ \s -> s { stateInterfaces = M.insert (stateModuleName s) exports $ stateInterfaces s }
  forM_ decls scanTypeSigs
  forM_ decls scanRecordDecls
  ifOptimizeNewtypes
    (forM_ decls scanNewtypeDecls)
    (return ())
preprocessAST () mod = throwError $ UnsupportedModuleSyntax "preprocessAST" mod

--------------------------------------------------------------------------------
-- | Preprocessing

-- | Find newtype declarations
scanNewtypeDecls :: F.Decl -> Compile ()
scanNewtypeDecls (DataDecl _ NewType{} _ _ constructors _) = compileNewtypeDecl constructors
scanNewtypeDecls _ = return ()

-- | Add new types to the state
compileNewtypeDecl :: [F.QualConDecl] -> Compile ()
compileNewtypeDecl [QualConDecl _ _ _ condecl] = case condecl of
    -- newtype declaration without destructor
  ConDecl _ name  [ty]            -> addNewtype name Nothing ty
  RecDecl _ cname [FieldDecl _ [dname] ty] -> addNewtype cname (Just dname) ty
  x -> error $ "compileNewtypeDecl case: Should be impossible (this is a bug). Got: " ++ show x
  where
    addNewtype cname dname ty = do
      qcname <- qualify cname
      qdname <- case dname of
                  Nothing -> return Nothing
                  Just n  -> Just <$> qualify n
      modify (\cs@CompileState{stateNewtypes=nts} ->
               cs{stateNewtypes=(qcname,qdname,unAnn ty):nts})
compileNewtypeDecl q = error $ "compileNewtypeDecl: Should be impossible (this is a bug). Got: " ++ show q

-- | Add record declarations to the state
scanRecordDecls :: F.Decl -> Compile ()
scanRecordDecls decl = do
  case decl of
    DataDecl _loc ty _ctx (F.declHeadName -> name) qualcondecls _deriv -> do
      let addIt = let ns = for qualcondecls (\(QualConDecl _loc' _tyvarbinds _ctx' condecl) -> conDeclName condecl)
                  in addRecordTypeState name ns
      case ty of
        DataType{} -> addIt
        NewType{} -> ifOptimizeNewtypes
                       (return ())
                       addIt
    _ -> return ()

  case decl of
    DataDecl _ ty _ _ constructors _ ->
      case ty of
        DataType{} -> dataDecl constructors
        NewType{} -> ifOptimizeNewtypes
                       (return ())
                       (dataDecl constructors)
    GDataDecl _ ty _ _ _ decls _ ->
      case ty of
        DataType{} -> dataDecl (map convertGADT decls)
        NewType{} -> ifOptimizeNewtypes
                       (return ())
                       (dataDecl (map convertGADT decls))
    _ -> return ()

  where
    addRecordTypeState (unAnn -> name') (map unAnn -> cons') = do
      name <- qualify name'
      cons <- mapM qualify cons'
      modify $ \s -> s { stateRecordTypes = (name, cons) : stateRecordTypes s }

    conDeclName (ConDecl _ n _) = n
    conDeclName (InfixConDecl _ _ n _) = n
    conDeclName (RecDecl _ n _) = n

    -- | Collect record definitions and store record name and field names.
    -- A ConDecl will have fields named slot1..slotN
    dataDecl :: [F.QualConDecl] -> Compile ()
    dataDecl constructors =
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
        addRecordState name' fields = do
          name <- qualify name'
          modify $ \s -> s
            { stateRecords = (name,map unAnn fields) : stateRecords s }

scanTypeSigs :: F.Decl -> Compile ()
scanTypeSigs decl = case decl of
  TypeSig _ names typ -> mapM_ (`addTypeSig` typ) names
  _ -> return ()
  where
    addTypeSig :: F.Name -> F.Type -> Compile ()
    addTypeSig (unAnn -> n') (unAnn -> t) = do
      n <- qualify n'
      modify $ \s -> s { stateTypeSigs = M.insert n t (stateTypeSigs s) }
