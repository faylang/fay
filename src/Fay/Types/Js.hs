-- | JS AST types

module Fay.Types.Js
  ( JsStmt (..)
  , JsExp (..)
  , JsLit (..)
  , JsName (..)
  ) where

import qualified Fay.Exts.NoAnnotation           as N
import           Fay.Types.ModulePath

import           Data.String
import           Language.Haskell.Exts

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | Literal value type.
data JsLit
  = JsChar Char
  | JsStr String
  | JsInt Int
  | JsFloating Double
  | JsBool Bool
  deriving (Eq, Show)

-- | Just handy to have.
instance IsString JsLit where fromString = JsStr
