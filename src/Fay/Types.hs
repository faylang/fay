{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- | All Fay types and instances.

module Fay.Types
  (JsStmt(..)
  ,JsExp(..)
  ,JsLit(..)
  ,JsName(..)
  ,CompileError(..)
  ,Compile(..)
  ,CompileModule
  ,Printable(..)
  ,Fay
  ,CompileReader(..)
  ,CompileWriter(..)
  ,Config(..)
  ,CompileState(..)
  ,FundamentalType(..)
  ,PrintState(..)
  ,defaultPrintState
  ,Printer(..)
  ,SerializeContext(..)
  ,ModulePath (unModulePath)
  ,mkModulePath
  ,mkModulePaths
  ,mkModulePathFromQName
  ) where

import           Fay.Compiler.Prelude

import           Fay.Config
import qualified Fay.Exts.NoAnnotation             as N
import qualified Fay.Exts.Scoped                   as S
import           Fay.Types.CompileError
import           Fay.Types.FFI
import           Fay.Types.Js
import           Fay.Types.ModulePath

import           Control.Monad.Error               (ErrorT, MonadError)
import           Control.Monad.Identity            (Identity)
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Map                          (Map)
import           Data.Set                          (Set)
import           Distribution.HaskellSuite.Modules
import           Language.Haskell.Names            (Symbols)
import           SourceMap.Types

--------------------------------------------------------------------------------
-- Compiler types

-- | State of the compiler.
data CompileState = CompileState
  -- TODO Change N.QName to GName? They can never be special so it would simplify.
  { stateInterfaces    :: Map N.ModuleName Symbols           -- ^ Exported identifiers for all modules
  , stateRecordTypes   :: [(N.QName,[N.QName])]              -- ^ Map types to constructors
  , stateRecords       :: [(N.QName,[N.Name])]               -- ^ Map constructors to fields
  , stateNewtypes      :: [(N.QName, Maybe N.QName, N.Type)] -- ^ Newtype constructor, destructor, wrapped type tuple
  , stateImported      :: [(N.ModuleName,FilePath)]          -- ^ Map of all imported modules and their source locations.
  , stateNameDepth     :: Integer                            -- ^ Depth of the current lexical scope, used for creating unshadowing variables.
  , stateModuleName    :: N.ModuleName                       -- ^ Name of the module currently being compiled.
  , stateJsModulePaths :: Set ModulePath                     -- ^ Module paths that have code generated for them.
  , stateUseFromString :: Bool                               -- ^ Use JS Strings instead of [Char] for string literals?
  , stateTypeSigs      :: Map N.QName N.Type                 -- ^ Module level declarations having explicit type signatures
  } deriving (Show)

-- | Things written out by the compiler.
data CompileWriter = CompileWriter
  { writerCons    :: [JsStmt]         -- ^ Constructors.
  , writerFayToJs :: [(String,JsExp)] -- ^ Fay to JS dispatchers.
  , writerJsToFay :: [(String,JsExp)] -- ^ JS to Fay dispatchers.
  } deriving (Show)

-- | Simple concatenating instance.
instance Monoid CompileWriter where
  mempty = CompileWriter [] [] []
  mappend (CompileWriter a b c) (CompileWriter x y z) =
    CompileWriter (a++x) (b++y) (c++z)

-- | Configuration and globals for the compiler.
data CompileReader = CompileReader
  { readerConfig       :: Config -- ^ The compilation configuration.
  , readerCompileLit   :: S.Sign -> S.Literal -> Compile JsExp
  , readerCompileDecls :: Bool -> [S.Decl] -> Compile [JsStmt]
  }

-- | Compile monad.
newtype Compile a = Compile
  { unCompile :: RWST CompileReader CompileWriter CompileState
                      (ErrorT CompileError (ModuleT (ModuleInfo Compile) IO))
                      a -- ^ Uns the compiler
  } deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError CompileError
    , MonadIO
    , MonadReader CompileReader
    , MonadState CompileState
    , MonadWriter CompileWriter
    )

type CompileModule a = ModuleT Symbols IO (Either CompileError (a, CompileState, CompileWriter))

instance MonadModule Compile where
  type ModuleInfo Compile = Symbols
  lookupInCache        = liftModuleT . lookupInCache
  insertInCache n m    = liftModuleT $ insertInCache n m
  getPackages          = liftModuleT getPackages
  readModuleInfo fps n = liftModuleT $ readModuleInfo fps n

liftModuleT :: ModuleT Symbols IO a -> Compile a
liftModuleT = Compile . lift . lift

-- | The state of the pretty printer.
data PrintState = PrintState
  { psPretty      :: Bool      -- ^ Are we to pretty print?
  , psLine        :: Int       -- ^ The current line.
  , psColumn      :: Int       -- ^ Current column.
  , psMappings    :: [Mapping] -- ^ Source mappings.
  , psIndentLevel :: Int       -- ^ Current indentation level.
  , psOutput      :: [String]  -- ^ The current output. TODO: Make more efficient.
  , psNewline     :: Bool      -- ^ Just outputted a newline?
  }

-- | Default state.
defaultPrintState :: PrintState
defaultPrintState = PrintState False 0 0 [] 0 [] False

-- | The printer monad.
newtype Printer a = Printer { runPrinter :: State PrintState a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadState PrintState
    )

-- | Print some value.
class Printable a where
  printJS :: a -> Printer ()

-- | The JavaScript FFI interfacing monad.
newtype Fay a = Fay (Identity a)
  deriving
    ( Applicative
    , Functor
    , Monad
    )
