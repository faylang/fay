{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | All Fay types and instances.

module Fay.Types
  (JsStmt(..)
  ,JsExp(..)
  ,JsLit(..)
  ,JsName(..)
  ,CompileError(..)
  ,Compile(..)
  ,CompilesTo(..)
  ,Printable(..)
  ,Fay
  ,CompileReader(..)
  ,CompileWriter(..)
  ,CompileConfig(..)
  ,CompileState(..)
  ,addCurrentExport
  ,getCurrentExports
  ,getExportsFor
  ,faySourceDir
  ,FundamentalType(..)
  ,PrintState(..)
  ,Printer(..)
  ,Mapping(..)
  ,SerializeContext(..))
  where

import           Control.Applicative
import           Control.Monad.Error    (Error, ErrorT, MonadError)
import           Control.Monad.Identity (Identity)
import           Control.Monad.State
import           Control.Monad.RWS
import           Data.Default
import           Data.Maybe
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.String
import           Language.Haskell.Exts
import           System.FilePath

import           Fay.Compiler.ModuleScope (ModuleScope)
import           Paths_fay

--------------------------------------------------------------------------------
-- Compiler types

-- | Configuration of the compiler.
data CompileConfig = CompileConfig
  { configOptimize           :: Bool                       -- ^ Run optimizations
  , configFlattenApps        :: Bool                       -- ^ Flatten function application?
  , configExportBuiltins     :: Bool                       -- ^ Export built-in functions?
  , configExportRuntime      :: Bool                       -- ^ Export the runtime?
  , configExportStdlib       :: Bool                       -- ^ Export the stdlib?
  , configExportStdlibOnly   :: Bool                       -- ^ Export /only/ the stdlib?
  , configDispatchers        :: Bool                       -- ^ Export dispatchers?
  , configDispatcherOnly     :: Bool                       -- ^ Export /only/ the dispatcher?
  , configNaked              :: Bool                       -- ^ Export without a module wrapper?
  , configDirectoryIncludes :: [(Maybe String, FilePath)]  -- ^ Possibly a fay package name, and a include directory.
  , configPrettyPrint        :: Bool                       -- ^ Pretty print the JS output?
  , configHtmlWrapper        :: Bool                       -- ^ Output a HTML file including the produced JS.
  , configHtmlJSLibs         :: [FilePath]                 -- ^ Any JS files to link to in the HTML.
  , configLibrary            :: Bool                       -- ^ Don't invoke main in the produced JS.
  , configWarn               :: Bool                       -- ^ Warn on dubious stuff, not related to typechecking.
  , configFilePath           :: Maybe FilePath             -- ^ File path to output to.
                                                           --   TODO: This flag is not used thoroughly, decide if it's needed.
  , configTypecheck          :: Bool                       -- ^ Typecheck with GHC.
  , configWall               :: Bool                       -- ^ Typecheck with -Wall.
  , configGClosure           :: Bool                       -- ^ Run Google Closure on the produced JS.
  , configPackageConf        :: Maybe FilePath             -- ^ The package config e.g. packages-6.12.3.
  , configPackages          :: [String]                    -- ^ Included Fay packages.
  } deriving (Show)

-- | State of the compiler.
data CompileState = CompileState
  { _stateExports     :: Map ModuleName (Set QName) -- ^ Collects exports from modules
  , stateRecordTypes  :: [(QName,[QName])]          -- ^ Map types to constructors
  , stateRecords      :: [(QName,[QName])]          -- ^ Map constructors to fields
  , stateNewtypes     :: [(QName, Maybe QName, Type)] -- ^ Newtype constructor, destructor, wrapped type tuple
  , stateImported     :: [(ModuleName,FilePath)]    -- ^ Map of all imported modules and their source locations.
  , stateNameDepth    :: Integer                    -- ^ Depth of the current lexical scope.
  , stateLocalScope   :: Set Name                   -- ^ Names in the current lexical scope.
  , stateModuleScope  :: ModuleScope                -- ^ Names in the module scope.
  , stateModuleName   :: ModuleName                 -- ^ Name of the module currently being compiled.
  } deriving (Show)

-- | Things written out by the compiler.
data CompileWriter = CompileWriter
  { writerCons     :: [JsStmt] -- ^ Constructors.
  , writerFayToJs  :: [JsStmt] -- ^ Fay to JS dispatchers.
  , writerJsToFay  :: [JsStmt] -- ^ JS to Fay dispatchers.
  }
  deriving (Show)

-- | Simple concatenating instance.
instance Monoid CompileWriter where
  mempty = CompileWriter [] [] []
  mappend (CompileWriter a b c) (CompileWriter x y z) =
    CompileWriter (a++x) (b++y) (c++z)

-- | Configuration and globals for the compiler.
data CompileReader = CompileReader
  { readerConfig       :: CompileConfig -- ^ The compilation configuration.
  , readerCompileLit   :: Literal -> Compile JsExp
  , readerCompileDecls :: Bool -> [Decl] -> Compile [JsStmt]
  }

-- | The data-files source directory.
faySourceDir :: IO FilePath
faySourceDir = fmap (takeDirectory . takeDirectory . takeDirectory) (getDataFileName "src/Language/Fay/Stdlib.hs")

-- | Adds a new export to '_stateExports' for the module specified by
-- 'stateModuleName'.
addCurrentExport :: QName -> CompileState -> CompileState
addCurrentExport q cs =
    cs { _stateExports = M.insert (stateModuleName cs) qnames $ _stateExports cs}
  where
    qnames = maybe (S.singleton q) (S.insert q)
           $ M.lookup (stateModuleName cs) (_stateExports cs)

-- | Get all of the exported identifiers for the current module.
getCurrentExports :: CompileState -> Set QName
getCurrentExports cs = getExportsFor (stateModuleName cs) cs

-- | Get all of the exported identifiers for the given module.
getExportsFor :: ModuleName -> CompileState -> Set QName
getExportsFor mn cs = excludeNewtypes cs $ fromMaybe S.empty $ M.lookup mn (_stateExports cs)
  where
    excludeNewtypes :: CompileState -> Set QName -> Set QName
    excludeNewtypes cs names =
      let newtypes = stateNewtypes cs
          constrs = map (\(c, _, _) -> c) newtypes
          destrs  = map (\(_, d, _) -> fromJust d) . filter (\(_, d, _) -> isJust d) $ newtypes
       in names `S.difference` (S.fromList constrs `S.union` S.fromList destrs)

-- | Compile monad.
newtype Compile a = Compile
  { unCompile :: RWST CompileReader CompileWriter CompileState (ErrorT CompileError IO) a -- ^ Run the compiler.
  }
  deriving (MonadState CompileState
           ,MonadError CompileError
           ,MonadReader CompileReader
           ,MonadWriter CompileWriter
           ,MonadIO
           ,Monad
           ,Functor
           ,Applicative)

-- | Just a convenience class to generalize the parsing/printing of
-- various types of syntax.
class (Parseable from,Printable to) => CompilesTo from to | from -> to where
  compileTo :: from -> Compile to

-- | A source mapping.
data Mapping = Mapping
  { mappingName :: String -- ^ The name of the mapping.
  , mappingFrom :: SrcLoc -- ^ The original source location.
  , mappingTo   :: SrcLoc -- ^ The new source location.
  } deriving (Show)

-- | The state of the pretty printer.
data PrintState = PrintState
  { psPretty       :: Bool      -- ^ Are we to pretty print?
  , psLine         :: Int       -- ^ The current line.
  , psColumn       :: Int       -- ^ Current column.
  , psMapping      :: [Mapping] -- ^ Source mappings.
  , psIndentLevel  :: Int       -- ^ Current indentation level.
  , psOutput       :: [String]  -- ^ The current output. TODO: Make more efficient.
  , psNewline      :: Bool      -- ^ Just outputted a newline?
  }

-- | Default state.
instance Default PrintState where
  def = PrintState False 0 0 [] 0 [] False

-- | The printer monad.
newtype Printer a = Printer { runPrinter :: State PrintState a }
  deriving (Monad,Functor,MonadState PrintState)

-- | Print some value.
class Printable a where
  printJS :: a -> Printer ()

-- | Error type.
data CompileError
  = ParseError SrcLoc String
  | UnsupportedDeclaration Decl
  | UnsupportedExportSpec ExportSpec
  | UnsupportedMatchSyntax Match
  | UnsupportedWhereInMatch Match
  | UnsupportedExpression Exp
  | UnsupportedLiteral Literal
  | UnsupportedLetBinding Decl
  | UnsupportedOperator QOp
  | UnsupportedPattern Pat
  | UnsupportedFieldPattern PatField
  | UnsupportedRhs Rhs
  | UnsupportedGuardedAlts GuardedAlts
  | UnsupportedWhereInAlt Alt
  | UnsupportedImport ImportDecl
  | UnsupportedQualStmt QualStmt
  | EmptyDoBlock
  | UnsupportedModuleSyntax Module
  | LetUnsupported
  | InvalidDoBlock
  | RecursiveDoUnsupported
  | Couldn'tFindImport ModuleName [FilePath]
  | FfiNeedsTypeSig Decl
  | FfiFormatBadChars SrcLoc String
  | FfiFormatNoSuchArg SrcLoc Int
  | FfiFormatIncompleteArg SrcLoc
  | FfiFormatInvalidJavaScript SrcLoc String String
  | UnableResolveUnqualified Name
  | UnableResolveQualified QName
  deriving (Show)
instance Error CompileError

-- | The JavaScript FFI interfacing monad.
newtype Fay a = Fay (Identity a)
  deriving Monad

--------------------------------------------------------------------------------
-- JS AST types

-- | Statement type.
data JsStmt
  = JsVar JsName JsExp
  | JsMappedVar SrcLoc JsName JsExp
  | JsIf JsExp [JsStmt] [JsStmt]
  | JsEarlyReturn JsExp
  | JsThrow JsExp
  | JsWhile JsExp [JsStmt]
  | JsUpdate JsName JsExp
  | JsSetProp JsName JsName JsExp
  | JsSetPropExtern JsName JsName JsExp
  | JsContinue
  | JsBlock [JsStmt]
  | JsExpStmt JsExp
  deriving (Show,Eq)

-- | Expression type.
data JsExp
  = JsName JsName
  | JsRawExp String
  | JsSeq [JsExp]
  | JsFun [JsName] [JsStmt] (Maybe JsExp)
  | JsLit JsLit
  | JsApp JsExp [JsExp]
  | JsNegApp JsExp
  | JsTernaryIf JsExp JsExp JsExp
  | JsNull
  | JsParen JsExp
  | JsGetProp JsExp JsName
  | JsLookup JsExp JsExp
  | JsUpdateProp JsExp JsName JsExp
  | JsGetPropExtern JsExp String
  | JsUpdatePropExtern JsExp JsName JsExp
  | JsList [JsExp]
  | JsNew JsName [JsExp]
  | JsThrowExp JsExp
  | JsInstanceOf JsExp JsName
  | JsIndex Int JsExp
  | JsEq JsExp JsExp
  | JsNeq JsExp JsExp
  | JsInfix String JsExp JsExp -- Used to optimize *, /, +, etc
  | JsObj [(String,JsExp)]
  | JsUndefined
  deriving (Show,Eq)

-- | A name of some kind.
data JsName
  = JsNameVar QName
  | JsThis
  | JsParametrizedType
  | JsThunk
  | JsForce
  | JsApply
  | JsParam Integer
  | JsTmp Integer
  | JsConstructor QName
  | JsBuiltIn Name
  deriving (Eq,Show)

-- | Literal value type.
data JsLit
  = JsChar Char
  | JsStr String
  | JsInt Int
  | JsFloating Double
  | JsBool Bool
  deriving (Show,Eq)

-- | Just handy to have.
instance IsString JsLit where fromString = JsStr

-- | These are the data types that are serializable directly to native
-- JS data types. Strings, floating points and arrays. The others are:
-- actions in the JS monad, which are thunks that shouldn't be forced
-- when serialized but wrapped up as JS zero-arg functions, and
-- unknown types can't be converted but should at least be forced.
data FundamentalType
   -- Recursive types.
 = FunctionType [FundamentalType]
 | JsType FundamentalType
 | ListType FundamentalType
 | TupleType [FundamentalType]
 | UserDefined Name [FundamentalType]
 | Defined FundamentalType
 | Nullable FundamentalType
 -- Simple types.
 | DateType
 | StringType
 | DoubleType
 | IntType
 | BoolType
 | PtrType
 --  Automatically serialize this type.
 | Automatic
 -- Unknown.
 | UnknownType
   deriving (Show)

-- | Helpful for some things.
instance IsString Name where
  fromString = Ident

-- | Helpful for some things.
instance IsString QName where
  fromString = UnQual . Ident

-- | Helpful for writing qualified symbols (Fay.*).
instance IsString ModuleName where
  fromString = ModuleName

-- | The serialization context indicates whether we're currently
-- serializing some value or a particular field in a user-defined data
-- type.
data SerializeContext = SerializeAnywhere | SerializeUserArg Int
  deriving (Read,Show,Eq)
