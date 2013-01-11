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
  ,configPackages
  ,addConfigPackage
  ,addConfigPackages
  ,CompileState(..)
  ,defaultCompileState
  ,faySourceDir
  ,FundamentalType(..)
  ,PrintState(..)
  ,Printer(..)
  ,NameScope(..)
  ,Mapping(..)
  ,SerializeContext(..))
  where

import           Control.Applicative
import           Control.Monad.Error    (Error, ErrorT, MonadError)
import           Control.Monad.Identity (Identity)
import           Control.Monad.State
import           Data.Default
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.String
import           Language.Haskell.Exts
import           System.FilePath

import           Paths_fay

--------------------------------------------------------------------------------
-- Compiler types

-- | Configuration of the compiler.
data CompileConfig = CompileConfig
  { configOptimize          :: Bool
  , configFlattenApps       :: Bool
  , configExportBuiltins    :: Bool
  , configExportRuntime     :: Bool
  , configExportStdlib      :: Bool
  , configExportStdlibOnly  :: Bool
  , configDispatchers       :: Bool
  , configDispatcherOnly    :: Bool
  , configNaked             :: Bool
  , _configDirectoryIncludes :: [FilePath]
  , configPrettyPrint       :: Bool
  , configHtmlWrapper       :: Bool
  , configHtmlJSLibs        :: [FilePath]
  , configLibrary           :: Bool
  , configWarn              :: Bool
  , configFilePath          :: Maybe FilePath
  , configTypecheck         :: Bool
  , configWall              :: Bool
  , configGClosure          :: Bool
  , configPackageConf       :: Maybe FilePath
  , _configPackages         :: [String]
  } deriving (Show)

-- | Default configuration.
instance Default CompileConfig where
  def =
    addConfigPackage "fay-base" $
      CompileConfig False False True True True False True False False [] False False [] False True Nothing True False False Nothing []

-- Restrict these setters so elements aren't accidentally removed.

configDirectoryIncludes :: CompileConfig -> [FilePath]
configDirectoryIncludes = _configDirectoryIncludes

addConfigDirectoryInclude :: FilePath -> CompileConfig -> CompileConfig
addConfigDirectoryInclude fp cfg = cfg { _configDirectoryIncludes = fp : _configDirectoryIncludes cfg }

addConfigDirectoryIncludes :: [FilePath] -> CompileConfig -> CompileConfig
addConfigDirectoryIncludes fps cfg = foldl (flip addConfigDirectoryInclude) cfg fps

configPackages :: CompileConfig -> [String]
configPackages = _configPackages

addConfigPackage :: String -> CompileConfig -> CompileConfig
addConfigPackage pkg cfg = cfg { _configPackages = pkg : _configPackages cfg }

addConfigPackages :: [String] -> CompileConfig -> CompileConfig
addConfigPackages fps cfg = foldl (flip addConfigPackage) cfg fps

-- | State of the compiler.
data CompileState = CompileState
  { stateConfig      :: CompileConfig
  , stateExports     :: [QName]
  , stateExportAll   :: Bool
  , stateModuleName  :: ModuleName
  , stateFilePath    :: FilePath
  , stateRecordTypes :: [(QName,[QName])] -- Map types to constructors
  , stateRecords     :: [(QName,[QName])] -- Map constructors to fields
  , stateFayToJs     :: [JsStmt]
  , stateJsToFay     :: [JsStmt]
  , stateImported    :: [(ModuleName,FilePath)]
  , stateNameDepth   :: Integer
  , stateScope       :: Map Name [NameScope]
} deriving (Show)

-- | A name's scope, either imported or bound locally.
data NameScope = ScopeImported ModuleName (Maybe Name)
               | ScopeImportedAs Bool ModuleName Name
               | ScopeBinding

  deriving (Show,Eq)

faySourceDir :: IO FilePath
faySourceDir = fmap (takeDirectory . takeDirectory . takeDirectory) (getDataFileName "src/Language/Fay/Stdlib.hs")

-- | The default compiler state.
defaultCompileState :: CompileConfig -> IO CompileState
defaultCompileState config = do
  srcdir <- faySourceDir
  types <- getDataFileName "src/Language/Fay/Types.hs"
  return $ CompileState {
    stateConfig = addConfigDirectoryInclude srcdir config
  , stateExports = []
  , stateExportAll = True
  , stateModuleName = ModuleName "Main"
  , stateRecordTypes = []
  , stateRecords = []
  , stateFayToJs = []
  , stateJsToFay = []
  , stateImported = [("Language.Fay.Types",types)]
  , stateNameDepth = 1
  , stateFilePath = "<unknown>"
  , stateScope = M.fromList primOps
  }

-- | The built-in operations that aren't actually compiled from
-- anywhere, they come from runtime.js.
--
-- They're in the names list so that they can be overriden by the user
-- in e.g. let a * b = a - b in 1 * 2.
--
-- So we resolve them to Fay$, i.e. the prefix used for the runtime
-- support. $ is not allowed in Haskell module names, so there will be
-- no conflicts if a user decicdes to use a module named Fay.
--
-- So e.g. will compile to (*) Fay$$mult, which is in runtime.js.
primOps :: [(Name, [NameScope])]
primOps =
  [(Symbol ">>",[ScopeImported "Fay$" (Just "then")])
  ,(Symbol ">>=",[ScopeImported "Fay$" (Just "bind")])
  ,(Ident "return",[ScopeImported "Fay$" (Just "return")])
  ,(Ident "force",[ScopeImported "Fay$" (Just "force")])
  ,(Ident "seq",[ScopeImported "Fay$" (Just "seq")])
  ,(Symbol "*",[ScopeImported "Fay$" (Just "mult")])
  ,(Symbol "+",[ScopeImported "Fay$" (Just "add")])
  ,(Symbol "-",[ScopeImported "Fay$" (Just "sub")])
  ,(Symbol "/",[ScopeImported "Fay$" (Just "div")])
  ,(Symbol "==",[ScopeImported "Fay$" (Just "eq")])
  ,(Symbol "/=",[ScopeImported "Fay$" (Just "neq")])
  ,(Symbol ">",[ScopeImported "Fay$" (Just "gt")])
  ,(Symbol "<",[ScopeImported "Fay$" (Just "lt")])
  ,(Symbol ">=",[ScopeImported "Fay$" (Just "gte")])
  ,(Symbol "<=",[ScopeImported "Fay$" (Just "lte")])
  ,(Symbol "&&",[ScopeImported "Fay$" (Just "and")])
  ,(Symbol "||",[ScopeImported "Fay$" (Just "or")])]

-- | Compile monad.
newtype Compile a = Compile { unCompile :: StateT CompileState (ErrorT CompileError IO) a }
  deriving (MonadState CompileState
           ,MonadError CompileError
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
