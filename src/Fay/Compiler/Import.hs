{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- | Handles finding imports and compiling them recursively.
-- This is done for each full AST traversal the copmiler does
-- which at this point is InitialPass's preprocessing
-- and Compiler's code generation
module Fay.Compiler.Import
  (startCompile
  ,compileWith
  ) where

import           Fay.Compiler.Config
import           Fay.Compiler.Misc
import           Fay.Control.Monad.IO
import qualified Fay.Exts                        as F
import           Fay.Exts.NoAnnotation           (unAnn)
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.RWS
import           Language.Haskell.Exts.Annotated hiding (name, var)
import           Prelude                         hiding (mod, read)
import           System.Directory
import           System.FilePath

-- | Start the compilation process using `compileModule` to compile a file.
startCompile :: (FilePath -> String -> Compile a) -> FilePath -> Compile a
startCompile compileModule filein = do
  modify $ \s -> s { stateImported = [] }
  fmap fst . listen $ compileModuleFromFile compileModule filein

-- | Compile a module
compileWith
  :: Monoid a
  => FilePath
  -> (a -> F.Module -> Compile a)
  -> (FilePath -> String -> Compile a)
  -> String
  -> Compile (a, CompileState, CompileWriter)
compileWith filepath with compileModule from = do
  rd <- ask
  st <- get
  res <- Compile . lift . lift $
    runCompileModule
      rd
      st
      (parseResult (throwError . uncurry ParseError)
                   (\mod@(Module _ _ _ imports _) -> do
                     res <- foldr (<>) mempty <$> mapM (compileImport compileModule) imports
                     modify $ \s -> s { stateModuleName = unAnn $ F.moduleName mod }
                     with res mod
                   )
                   (parseFay filepath from))
  either throwError return res

-- | Compile a module given its file path
compileModuleFromFile
  :: (FilePath -> String -> Compile a)
  -> FilePath
  -> Compile a
compileModuleFromFile compileModule fp = io (readFile fp) >>= compileModule fp

-- | Lookup a module from include directories and compile.
compileModuleFromName
  :: Monoid a
  => (FilePath -> String -> Compile a)
  -> F.ModuleName
  -> Compile a
compileModuleFromName compileModule nm =
  unlessImported nm compileModule
    where
      unlessImported
        :: Monoid a
        => ModuleName l
        -> (FilePath -> String -> Compile a)
        -> Compile a
      unlessImported (ModuleName _ "Fay.Types") _ = return mempty
      unlessImported (unAnn -> name) importIt = do
        imported <- gets stateImported
        case lookup name imported of
          Just _  -> return mempty
          Nothing -> do
            dirs <- configDirectoryIncludePaths <$> config id
            (filepath,contents) <- findImport dirs name
            modify $ \s -> s { stateImported = (name,filepath) : imported }
            importIt filepath contents

-- | Compile an import.
compileImport
  :: Monoid a
  => (FilePath -> String -> Compile a)
  -> F.ImportDecl
  -> Compile a
compileImport compileModule i = case i of
  -- Package imports are ignored since they are used for some trickery in fay-base.
  ImportDecl _ _    _ _ Just{}  _ _ -> return mempty
  ImportDecl _ name _ _ Nothing _ _ -> compileModuleFromName compileModule name

-- | Find an import's filepath and contents from its module name.
findImport :: [FilePath] -> ModuleName a -> Compile (FilePath,String)
findImport alldirs (unAnn -> mname) = go alldirs mname where
  go :: [FilePath] -> ModuleName a -> Compile (FilePath,String)
  go _ (ModuleName _ "Fay.Types") = return ("Fay/Types.hs", "newtype Fay a = Fay (Identity a)\n\nnewtype Identity a = Identity a")
  go (dir:dirs) name = do
    exists <- io (doesFileExist path)
    if exists
      then (path,) . stdlibHack <$> io (readFile path)
      else go dirs name
    where
      path = dir </> replace '.' '/' (prettyPrint name) ++ ".hs"
      replace c r = map (\x -> if x == c then r else x)
  go [] name =
    throwError $ Couldn'tFindImport (unAnn name) alldirs

  stdlibHack = case mname of
    ModuleName _ "Fay.FFI" -> const "module Fay.FFI where\n\ndata Nullable a = Nullable a | Null\n\ndata Defined a = Defined a | Undefined"
    _ -> id
