{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | All Fay types and instances.

module Language.Fay.Types
  (JsStmt(..)
  ,JsExp(..)
  ,JsLit(..)
  ,JsParam
  ,JsName
  ,CompileError(..)
  ,Compile
  ,CompilesTo(..)
  ,Printable(..)
  ,Fay)
  where

import Control.Exception
import Control.Monad.Error (Error,ErrorT)
import Control.Monad.Identity (Identity)
import Data.Data
import Language.Haskell.Exts

--------------------------------------------------------------------------------
-- Compiler types

-- | Convenience/doc type.
type Compile = ErrorT CompileError IO

-- | Convenience type for function parameters.
type JsParam = JsName

-- | To be used to force name sanitization eventually.
type JsName = QName -- FIXME: Force sanitization at this point.

-- | Just a convenience class to generalize the parsing/printing of
-- various types of syntax.
class (Parseable from,Printable to) => CompilesTo from to | from -> to where
  compileTo :: from -> Compile to

-- | Print some value.
class Printable a where
  printJS :: a -> String

-- | Error type.
data CompileError
  = ParseError SrcLoc String
  | UnsupportedDeclaration Decl
  | UnsupportedMatchSyntax Match
  | UnsupportedExpression Exp
  | UnsupportedLiteral Literal
  | UnsupportedLetBinding Decl
  | UnsupportedOperator QOp
  | UnsupportedPattern Pat
  | UnsupportedRhs Rhs
  | UnsupportedGuardedAlts GuardedAlts
  | EmptyDoBlock
  | UnsupportedModuleSyntax Module
  | LetUnsupported
  | InvalidDoBlock
  | RecursiveDoUnsupported
  | FfiNeedsTypeSig Decl
  deriving (Show,Eq,Data,Typeable)
instance Error CompileError
instance Exception CompileError

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
  deriving (Show,Eq)
  
-- | Expression type.
data JsExp
  = JsName JsName
  | JsRawName String
  | JsFun [JsParam] [JsStmt] (Maybe JsExp)
  | JsLit JsLit
  | JsApp JsExp [JsExp]
  | JsTernaryIf JsExp JsExp JsExp
  | JsNull
  | JsSequence [JsExp]
  | JsParen JsExp
  | JsGetProp JsExp JsName
  | JsList [JsExp]
  | JsNew JsName [JsExp]
  | JsThrowExp JsExp
  | JsInstanceOf JsExp JsName
  | JsIndex Int JsExp
  | JsEq JsExp JsExp
  | JsInfix String JsExp JsExp -- Used to optimize *, /, +, etc
  deriving (Show,Eq)

-- | Literal value type.
data JsLit
  = JsChar Char
  | JsStr String
  | JsInt Int
  | JsFloating Double
  | JsBool Bool
  deriving (Show,Eq)
