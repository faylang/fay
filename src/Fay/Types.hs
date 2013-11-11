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
  ,CompileResult
  ,CompileModule
  ,Printable(..)
  ,Fay
  ,CompileReader(..)
  ,CompileWriter(..)
  ,CompileConfig(..)
  ,CompileState(..)
  ,FundamentalType(..)
  ,PrintState(..)
  ,Printer(..)
  ,SerializeContext(..)
  ,ModulePath (unModulePath)
  ,mkModulePath
  ,mkModulePaths
  ,mkModulePathFromQName
  ) where

import           Fay.Compiler.QName
import qualified Fay.Exts                          as F
import qualified Fay.Exts.NoAnnotation             as N
import qualified Fay.Exts.Scoped                   as S

import           Control.Applicative
import           Control.Monad.Error               (Error, ErrorT, MonadError)
import           Control.Monad.Identity            (Identity)
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Default
import           Data.List
import           Data.List.Split
import           Data.Map                          (Map)
import           Data.Set                          (Set)
import           Data.String
import           Distribution.HaskellSuite.Modules
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names            (Symbols)
import           SourceMap.Types

--------------------------------------------------------------------------------
-- Compiler types

-- | Configuration of the compiler.
data CompileConfig = CompileConfig
  { configOptimize          :: Bool                        -- ^ Run optimizations
  , configFlattenApps       :: Bool                        -- ^ Flatten function application?
  , configExportRuntime     :: Bool                        -- ^ Export the runtime?
  , configExportStdlib      :: Bool                        -- ^ Export the stdlib?
  , configExportStdlibOnly  :: Bool                        -- ^ Export /only/ the stdlib?
  , configDirectoryIncludes :: [(Maybe String, FilePath)]  -- ^ Possibly a fay package name, and a include directory.
  , configPrettyPrint       :: Bool                        -- ^ Pretty print the JS output?
  , configHtmlWrapper       :: Bool                        -- ^ Output a HTML file including the produced JS.
  , configSourceMap         :: Bool                        -- ^ Output a source map file as outfile.map.
  , configHtmlJSLibs        :: [FilePath]                  -- ^ Any JS files to link to in the HTML.
  , configLibrary           :: Bool                        -- ^ Don't invoke main in the produced JS.
  , configWarn              :: Bool                        -- ^ Warn on dubious stuff, not related to typechecking.
  , configFilePath          :: Maybe FilePath              -- ^ File path to output to.
  , configTypecheck         :: Bool                        -- ^ Typecheck with GHC.
  , configWall              :: Bool                        -- ^ Typecheck with -Wall.
  , configGClosure          :: Bool                        -- ^ Run Google Closure on the produced JS.
  , configPackageConf       :: Maybe FilePath              -- ^ The package config e.g. packages-6.12.3.
  , configPackages          :: [String]                    -- ^ Included Fay packages.
  , configBasePath          :: Maybe FilePath              -- ^ Custom source location for fay-base
  , configStrict            :: [String]                    -- ^ Produce strict and uncurried wrappers for all functions with type signatures in the given module
  , configTypecheckOnly     :: Bool                        -- ^ Only invoke GHC for typechecking, don't produce any output
  , configRuntimePath       :: Maybe FilePath
  } deriving (Show)

-- | The name of a module split into a list for code generation.
newtype ModulePath = ModulePath { unModulePath :: [String] }
  deriving (Eq, Ord, Show)

-- | Construct the complete ModulePath from a ModuleName.
mkModulePath :: ModuleName a -> ModulePath
mkModulePath (ModuleName _ m) = ModulePath . splitOn "." $ m

-- | Construct intermediate module paths from a ModuleName.
-- mkModulePaths "A.B" => [["A"], ["A","B"]]
mkModulePaths :: ModuleName a -> [ModulePath]
mkModulePaths (ModuleName _ m) = map ModulePath . tail . inits . splitOn "." $ m

-- | Converting a QName to a ModulePath is only relevant for constructors since
-- they can conflict with module names.
mkModulePathFromQName :: QName a -> ModulePath
mkModulePathFromQName (Qual _ (ModuleName _ m) n) = mkModulePath $ ModuleName F.noI $ m ++ "." ++ unname n
mkModulePathFromQName _ = error "mkModulePathFromQName: Not a qualified name"

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
  , readerCompileLit   :: S.Literal -> Compile JsExp
  , readerCompileDecls :: Bool -> [S.Decl] -> Compile [JsStmt]
  }

-- | Compile monad.
newtype Compile a = Compile
  { unCompile :: (RWST CompileReader
                      CompileWriter
                      CompileState
                      (ErrorT CompileError (ModuleT (ModuleInfo Compile) IO)))
                   a -- ^ Uns the compiler
  }
  deriving (MonadState CompileState
           ,MonadError CompileError
           ,MonadReader CompileReader
           ,MonadWriter CompileWriter
           ,MonadIO
           ,Monad
           ,Functor
           ,Applicative
           )

type CompileResult a
  = Either CompileError
           (a, CompileState, CompileWriter)

type CompileModule a
  = ModuleT Symbols
            IO
            (CompileResult a)

instance MonadModule Compile where
  type ModuleInfo Compile = Symbols
  lookupInCache        = liftModuleT . lookupInCache
  insertInCache n m    = liftModuleT $ insertInCache n m
  getPackages          = liftModuleT $ getPackages
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
  = Couldn'tFindImport N.ModuleName [FilePath]
  | EmptyDoBlock
  | FfiFormatBadChars SrcSpanInfo String
  | FfiFormatIncompleteArg SrcSpanInfo
  | FfiFormatInvalidJavaScript SrcSpanInfo String String
  | FfiFormatNoSuchArg SrcSpanInfo Int
  | FfiNeedsTypeSig S.Decl
  | GHCError String
  | InvalidDoBlock
  | ParseError S.SrcLoc String
  | ShouldBeDesugared String
  | UnableResolveQualified N.QName
  | UnsupportedDeclaration S.Decl
  | UnsupportedEnum N.Exp
  | UnsupportedExportSpec N.ExportSpec
  | UnsupportedExpression S.Exp
  | UnsupportedFieldPattern S.PatField
  | UnsupportedImport F.ImportDecl
  | UnsupportedLet
  | UnsupportedLetBinding S.Decl
  | UnsupportedLiteral S.Literal
  | UnsupportedModuleSyntax String F.Module
  | UnsupportedPattern S.Pat
  | UnsupportedQualStmt S.QualStmt
  | UnsupportedRecursiveDo
  | UnsupportedRhs S.Rhs
  | UnsupportedWhereInAlt S.Alt
  | UnsupportedWhereInMatch S.Match
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
  | JsIf JsExp [JsStmt] [JsStmt]
  | JsEarlyReturn JsExp
  | JsThrow JsExp
  | JsWhile JsExp [JsStmt]
  | JsUpdate JsName JsExp
  | JsSetProp JsName JsName JsExp
  | JsSetQName (Maybe SrcSpan) N.QName JsExp
  | JsSetModule ModulePath JsExp
  | JsSetConstructor N.QName JsExp
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
  | JsFun (Maybe JsName) [JsName] [JsStmt] (Maybe JsExp)
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
  | JsLitObj [(N.Name,JsExp)]
  | JsUndefined
  | JsAnd JsExp JsExp
  | JsOr  JsExp JsExp
  deriving (Show,Eq)

-- | A name of some kind.
data JsName
  = JsNameVar N.QName
  | JsThis
  | JsParametrizedType
  | JsThunk
  | JsForce
  | JsApply
  | JsParam Integer
  | JsTmp Integer
  | JsConstructor N.QName
  | JsBuiltIn N.Name
  | JsModuleName N.ModuleName
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
 | UserDefined N.Name [FundamentalType]
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

-- | The serialization context indicates whether we're currently
-- serializing some value or a particular field in a user-defined data
-- type.
data SerializeContext = SerializeAnywhere | SerializeUserArg Int
  deriving (Read,Show,Eq)
