{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | All Fay types and instances.

module Language.Fay.Types
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
  ,CompileConfig(
     configFlattenApps
    ,configOptimize
    ,configGClosure
    ,configExportBuiltins
    ,configExportRuntime
    ,configNaked
    ,configPrettyPrint
    ,configHtmlWrapper
    ,configHtmlJSLibs
    ,configLibrary
    ,configWarn
    ,configFilePath
    ,configTypecheck
    ,configWall
    ,configPackageConf
    ,configExportStdlib
    ,configDispatchers
    ,configDispatcherOnly
    ,configExportStdlibOnly
  )
  ,configDirectoryIncludes
  ,addConfigDirectoryInclude
  ,addConfigDirectoryIncludes
  ,addConfigDirectoryIncludePaths
  ,configDirectoryIncludePaths
  ,nonPackageConfigDirectoryIncludePaths
  ,configPackages
  ,addConfigPackage
  ,addConfigPackages
  ,CompileState(..)
  ,addCurrentExport
  ,getCurrentExports
  ,getExportsFor
  ,defaultCompileState
  ,defaultCompileReader
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

import           Language.Fay.ModuleScope (ModuleScope)
import           Paths_fay

--------------------------------------------------------------------------------
-- Compiler types

-- | Configuration of the compiler.
data CompileConfig = CompileConfig
  { configOptimize           :: Bool                       -- ^ Run optimizations
  , configFlattenApps        :: Bool
  , configExportBuiltins     :: Bool
  , configExportRuntime      :: Bool
  , configExportStdlib       :: Bool
  , configExportStdlibOnly   :: Bool
  , configDispatchers        :: Bool
  , configDispatcherOnly     :: Bool
  , configNaked              :: Bool
  , _configDirectoryIncludes :: [(Maybe String, FilePath)] -- ^ Possibly a fay package name, and a include directory.
  , configPrettyPrint        :: Bool
  , configHtmlWrapper        :: Bool                       -- ^ Output a HTML file including the produced JS.
  , configHtmlJSLibs         :: [FilePath]
  , configLibrary            :: Bool                       -- ^ Don't invoke main in the produced JS.
  , configWarn               :: Bool                       -- ^ Warn on dubious stuff, not related to typechecking.
  , configFilePath           :: Maybe FilePath             -- TODO This flag is not used thoroughly, decide if it's needed.
  , configTypecheck          :: Bool                       -- ^ Typecheck with GHC
  , configWall               :: Bool                       -- ^ Typecheck with -Wall
  , configGClosure           :: Bool                       -- ^ Run Google Closure on the produced JS.
  , configPackageConf        :: Maybe FilePath
  , _configPackages          :: [String]                   -- ^ Included Fay packages
  } deriving (Show)

-- | Default configuration.
instance Default CompileConfig where
  def =
    addConfigPackage "fay-base" $
      CompileConfig
      { configOptimize           = False
      , configFlattenApps        = False
      , configExportBuiltins     = True
      , configExportRuntime      = True
      , configExportStdlib       = True
      , configExportStdlibOnly   = False
      , configDispatchers        = True
      , configDispatcherOnly     = False
      , configNaked              = False
      , _configDirectoryIncludes = []
      , configPrettyPrint        = False
      , configHtmlWrapper        = False
      , configHtmlJSLibs         = []
      , configLibrary            = False
      , configWarn               = True
      , configFilePath           = Nothing
      , configTypecheck          = True
      , configWall               = False
      , configGClosure           = False
      , configPackageConf        = Nothing
      , _configPackages          = []
      }

-- | Mapping of packages to source directories, a fst Nothing means the include
-- was added without a package reference.
configDirectoryIncludes :: CompileConfig -> [(Maybe String, FilePath)]
configDirectoryIncludes = _configDirectoryIncludes

-- | Get all include directories without the package mapping.
configDirectoryIncludePaths :: CompileConfig -> [FilePath]
configDirectoryIncludePaths = map snd . configDirectoryIncludes

-- | Get all include directories not included through packages.
nonPackageConfigDirectoryIncludePaths :: CompileConfig -> [FilePath]
nonPackageConfigDirectoryIncludePaths = map snd . filter (isJust . fst) . configDirectoryIncludes

-- | Add a mapping from (maybe) a package to a source directory
addConfigDirectoryInclude :: Maybe String -> FilePath -> CompileConfig -> CompileConfig
addConfigDirectoryInclude pkg fp cfg = cfg { _configDirectoryIncludes = (pkg, fp) : _configDirectoryIncludes cfg }

-- | Add several include directories.
addConfigDirectoryIncludes :: [(Maybe String,FilePath)] -> CompileConfig -> CompileConfig
addConfigDirectoryIncludes pkgFps cfg = foldl (\c (pkg,fp) -> addConfigDirectoryInclude pkg fp c) cfg pkgFps

-- | Add several include directories without package references.
addConfigDirectoryIncludePaths :: [FilePath] -> CompileConfig -> CompileConfig
addConfigDirectoryIncludePaths fps cfg = foldl (flip (addConfigDirectoryInclude Nothing)) cfg fps

-- | All included packages
configPackages :: CompileConfig -> [String]
configPackages = _configPackages

-- | Add a package to compilation
addConfigPackage :: String -> CompileConfig -> CompileConfig
addConfigPackage pkg cfg = cfg { _configPackages = pkg : _configPackages cfg }

-- | Add several packages to compilation
addConfigPackages :: [String] -> CompileConfig -> CompileConfig
addConfigPackages fps cfg = foldl (flip addConfigPackage) cfg fps

-- | State of the compiler.
data CompileState = CompileState
  { _stateExports     :: Map ModuleName (Set QName) -- ^ Collects exports from modules
  , stateModuleName   :: ModuleName                 -- ^ Name of the module currently being compiled.
  , stateFilePath     :: FilePath
  , stateRecordTypes  :: [(QName,[QName])]          -- ^ Map types to constructors
  , stateRecords      :: [(QName,[QName])]          -- ^ Map constructors to fields
  , stateFayToJs      :: [JsStmt]
  , stateJsToFay      :: [JsStmt]
  , stateImported     :: [(ModuleName,FilePath)]    -- ^ Map of all imported modules and their source locations.
  , stateNameDepth    :: Integer                    -- ^ Depth of the current lexical scope.
  , stateLocalScope   :: Set Name                   -- ^ Names in the current lexical scope.
  , stateModuleScope  :: ModuleScope                -- ^ Names in the module scope.
  , stateCons        :: [JsStmt]
} deriving (Show)

data CompileReader = CompileReader
  { readerConfig :: CompileConfig
  } deriving (Show)

faySourceDir :: IO FilePath
faySourceDir = fmap (takeDirectory . takeDirectory . takeDirectory) (getDataFileName "src/Language/Fay/Stdlib.hs")

-- | The default compiler reader value.
defaultCompileReader :: CompileConfig -> IO CompileReader
defaultCompileReader config = do
  srcdir <- faySourceDir
  return CompileReader
    { readerConfig = addConfigDirectoryInclude Nothing srcdir config
    }

-- | The default compiler state.
defaultCompileState :: IO CompileState
defaultCompileState = do
  types <- getDataFileName "src/Language/Fay/Types.hs"
  return $ CompileState {
    _stateExports = M.empty
  , stateModuleName = ModuleName "Main"
  , stateRecordTypes = []
  , stateRecords = []
  , stateFayToJs = []
  , stateJsToFay = []
  , stateImported = [("Language.Fay.Types",types)]
  , stateNameDepth = 1
  , stateFilePath = "<unknown>"
  , stateLocalScope = S.empty
  , stateModuleScope = def
  , stateCons = []
  }

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
getExportsFor mn cs = fromMaybe S.empty $ M.lookup mn (_stateExports cs)

-- | Compile monad.
newtype Compile a = Compile
  { unCompile :: RWST CompileReader
                      ()
                      CompileState
                      (ErrorT CompileError IO)
                      a
  }
  deriving (MonadState CompileState
           ,MonadError CompileError
           ,MonadReader CompileReader
           ,MonadIO
           ,Monad
           ,Functor
           ,Applicative)

-- | Just a convenience class to generalize the parsing/printing of
-- various types of syntax.
class (Parseable from,Printable to) => CompilesTo from to | from -> to where
  compileTo :: from -> Compile to

data Mapping = Mapping
  { mappingName :: String
  , mappingFrom :: SrcLoc
  , mappingTo   :: SrcLoc
  } deriving (Show)

data PrintState = PrintState
  { psPretty       :: Bool
  , psLine         :: Int
  , psColumn       :: Int
  , psMapping      :: [Mapping]
  , psIndentLevel  :: Int
  , psOutput       :: [String]
  , psNewline      :: Bool
  }

instance Default PrintState where
  def = PrintState False 0 0 [] 0 [] False

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
  | FfiFormatBadChars String
  | FfiFormatNoSuchArg Int
  | FfiFormatIncompleteArg
  | FfiFormatInvalidJavaScript SrcLoc String String
  | UnableResolveUnqualified Name
  | UnableResolveQualified QName
  | UnableResolveCachedImport ModuleName
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

data SerializeContext = SerializeAnywhere | SerializeUserArg Int
  deriving (Read,Show,Eq)
