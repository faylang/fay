{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | All Fay types and instances.

module Language.Fay.Types
  (JsStmt(..)
  ,JsExp(..)
  ,JsLit(..)
  ,JsParam
  ,JsName
  ,CompileError(..)
  ,Compile(..)
  ,CompilesTo(..)
  ,Printable(..)
  ,Fay
  ,CompileConfig(..)
  ,CompileState(..)
  ,defaultCompileState
  ,FundamentalType(..)
  ,PrintState(..)
  ,Printer(..))
  where

import           Control.Applicative

import           Control.Monad.Error    (Error, ErrorT, MonadError)
import           Control.Monad.Identity (Identity)
import           Control.Monad.State

import           Data.Default
import           Language.Haskell.Exts

--------------------------------------------------------------------------------
-- Compiler types

-- | Configuration of the compiler.
data CompileConfig = CompileConfig
  { configTCO               :: Bool
  , configFlattenApps       :: Bool
  , configExportBuiltins    :: Bool
  , configDirectoryIncludes :: [FilePath]
  , configPrettyPrint       :: Bool
  , configHtmlWrapper       :: Bool
  , configHtmlJSLibs        :: [FilePath]
  , configLibrary           :: Bool
  , configWarn              :: Bool
  , configFilePath          :: Maybe FilePath
  , configTypecheck         :: Bool
  , configWall              :: Bool
  }

-- | Default configuration.
instance Default CompileConfig where
  def = CompileConfig False False True [] False False [] False True Nothing True False

-- | State of the compiler.
data CompileState = CompileState
  { stateConfig      :: CompileConfig
  , stateExports     :: [Name]
  , stateExportAll   :: Bool
  , stateModuleName  :: ModuleName
  , stateFilePath    :: FilePath
  , stateRecords     :: [(Name,[Name])] -- records with field names
  , stateFayToJs     :: [JsStmt]
  , stateJsToFay     :: [JsStmt]
  , stateImported    :: [String] -- ^ Names of imported modules so far.
  , stateNameDepth   :: Integer
}

defaultCompileState :: CompileConfig -> CompileState
defaultCompileState config = CompileState {
    stateConfig = config
  , stateExports = []
  , stateExportAll = True
  , stateModuleName = ModuleName "Main"
  , stateRecords = [(Ident "Nothing",[]),(Ident "Just",[Ident "slot1"])]
  , stateFayToJs = []
  , stateJsToFay = []
  , stateImported = ["Language.Fay.Prelude","Language.Fay.FFI","Language.Fay.Types","Prelude"]
  , stateNameDepth = 1
  , stateFilePath = "<unknown>"
  }

-- | Compile monad.
newtype Compile a = Compile { unCompile :: StateT CompileState (ErrorT CompileError IO) a }
  deriving (MonadState CompileState
           ,MonadError CompileError
           ,MonadIO
           ,Monad
           ,Functor
           ,Applicative)

-- | Convenience type for function parameters.
type JsParam = JsName

-- | To be used to force name sanitization eventually.
type JsName = QName -- FIXME: Force sanitization at this point.

-- | Just a convenience class to generalize the parsing/printing of
-- various types of syntax.
class (Parseable from,Printable to) => CompilesTo from to | from -> to where
  compileTo :: from -> Compile to

data PrintState = PrintState
  { psLine         :: Int
  , psColumn       :: Int
  , psMapping      :: [(SrcLoc,SrcLoc)]
  , psIndentLevel  :: Int
  , psOutput       :: [String]
  }

instance Default PrintState where
  def = PrintState 0 0 [] 0 []

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
  | UnsupportedImport ImportDecl
  | UnsupportedQualStmt QualStmt
  | EmptyDoBlock
  | UnsupportedModuleSyntax Module
  | LetUnsupported
  | InvalidDoBlock
  | RecursiveDoUnsupported
  | Couldn'tFindImport String [FilePath]
  | FfiNeedsTypeSig Decl
  | FfiFormatBadChars String
  | FfiFormatNoSuchArg Int
  | FfiFormatIncompleteArg
  | FfiFormatInvalidJavaScript String String
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
  | JsContinue
  | JsBlock [JsStmt]
  deriving (Show,Eq)

-- | Expression type.
data JsExp
  = JsName JsName
  | JsRawExp String
  | JsFun [JsParam] [JsStmt] (Maybe JsExp)
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
  | JsInfix String JsExp JsExp -- Used to optimize *, /, +, etc
  | JsObj [(String,JsExp)]
  deriving (Show,Eq)

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
-- actiosn in the JS monad, which are thunks that shouldn't be forced
-- when serialized but wrapped up as JS zero-arg functions, and
-- unknown types can't be converted but should at least be forced.
data FundamentalType
   -- Recursive types.
 = FunctionType [FundamentalType]
 | JsType FundamentalType
 | ListType FundamentalType
 | UserDefined Name [FundamentalType]
 -- Simple types.
 | DateType
 | StringType
 | DoubleType
 | IntType
 | BoolType
 -- | Unknown.
 | UnknownType
   deriving (Show)
