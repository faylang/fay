{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans  #-}

-- | Miscellaneous functions used throughout the compiler.

module Language.Fay.Compiler.Misc where

import Language.Fay.Types

import Control.Monad.Error
import Control.Monad.State
import Data.List
import Data.String
import Language.Haskell.Exts (ParseResult(..))
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)

-- | Extra the string from an ident.
unname :: Name -> String
unname (Ident str) = str
unname _ = error "Expected ident from uname." -- FIXME:

-- | Make an identifier from the built-in HJ module.
fayBuiltin :: String -> QName
fayBuiltin = Qual (ModuleName "Fay") . Ident

-- | Wrap an expression in a thunk.
thunk :: JsExp -> JsExp
-- thunk exp = JsNew (fayBuiltin "Thunk") [JsFun [] [] (Just exp)]
thunk expr =
  case expr of
    -- JS constants don't need to be in thunks, they're already strict.
    JsLit{} -> expr
    -- Functions (e.g. lets) used for introducing a new lexical scope
    -- aren't necessary inside a thunk. This is a simple aesthetic
    -- optimization.
    JsApp fun@JsFun{} [] -> JsNew JsThunk [fun]
    -- Otherwise make a regular thunk.
    _ -> JsNew JsThunk [JsFun [] [] (Just expr)]

-- | Wrap an expression in a thunk.
stmtsThunk :: [JsStmt] -> JsExp
stmtsThunk stmts = JsNew JsThunk [JsFun [] stmts Nothing]

-- | Generate unique names.
uniqueNames :: [JsName]
uniqueNames = map JsParam [1::Integer ..]

-- | Make a top-level binding.
bindToplevel :: SrcLoc -> Bool -> QName -> JsExp -> Compile JsStmt
bindToplevel srcloc toplevel name expr = do
  exportAll <- gets stateExportAll
  when (toplevel && exportAll) $ emitExport (EVar name)
  return (JsMappedVar srcloc (JsNameVar name) expr)

-- | Emit exported names.
emitExport :: ExportSpec -> Compile ()
emitExport spec =
  case spec of
    EVar name -> modify $ \s -> s { stateExports = name : stateExports s }
    _ -> throwError (UnsupportedExportSpec spec)

-- | Force an expression in a thunk.
force :: JsExp -> JsExp
force expr
  | isConstant expr = expr
  | otherwise = JsApp (JsName JsForce) [expr]

-- | Is a JS expression a literal (constant)?
isConstant :: JsExp -> Bool
isConstant JsLit{} = True
isConstant _       = False

-- | Extract the string from a qname.
-- qname :: QName -> String
-- qname (UnQual (Ident str)) = str
-- qname (UnQual (Symbol sym)) = jsEncodeName sym
-- qname i = error $ "qname: Expected unqualified ident, found: " ++ show i -- FIXME:

-- | Deconstruct a parse result (a la maybe, foldr, either).
parseResult :: ((SrcLoc,String) -> b) -> (a -> b) -> ParseResult a -> b
parseResult die ok result =
  case result of
    ParseOk a -> ok a
    ParseFailed srcloc msg -> die (srcloc,msg)

-- | Get a config option.
config :: (CompileConfig -> a) -> Compile a
config f = gets (f . stateConfig)

-- | Optimize pattern matching conditions by merging conditions in common.
optimizePatConditions :: [[JsStmt]] -> [[JsStmt]]
optimizePatConditions = concatMap merge . groupBy sameIf where
  sameIf [JsIf cond1 _ _] [JsIf cond2 _ _] = cond1 == cond2
  sameIf _ _ = False
  merge xs@([JsIf cond _ _]:_) =
    [[JsIf cond (concat (optimizePatConditions (map getIfConsequent xs))) []]]
  merge noifs = noifs
  getIfConsequent [JsIf _ cons _] = cons
  getIfConsequent other = other

-- | Throw a JS exception.
throw :: String -> JsExp -> JsStmt
throw msg expr = JsThrow (JsList [JsLit (JsStr msg),expr])

-- | Throw a JS exception (in an expression).
throwExp :: String -> JsExp -> JsExp
throwExp msg expr = JsThrowExp (JsList [JsLit (JsStr msg),expr])

-- | Is an alt a wildcard?
isWildCardAlt :: Alt -> Bool
isWildCardAlt (Alt _ pat _ _) = isWildCardPat pat

-- | Is a pattern a wildcard?
isWildCardPat :: Pat -> Bool
isWildCardPat PWildCard{} = True
isWildCardPat PVar{}      = True
isWildCardPat _           = False

-- | Generate a temporary, SCOPED name for testing conditions and
-- such.
withScopedTmpJsName :: (JsName -> Compile a) -> Compile a
withScopedTmpJsName withName = do
  depth <- gets stateNameDepth
  modify $ \s -> s { stateNameDepth = depth + 1 }
  ret <- withName $ JsTmp depth
  modify $ \s -> s { stateNameDepth = depth }
  return ret

-- | Generate a temporary, SCOPED name for testing conditions and
-- such. We don't have name tracking yet, so instead we use this.
withScopedTmpName :: (Name -> Compile a) -> Compile a
withScopedTmpName withName = do
  depth <- gets stateNameDepth
  modify $ \s -> s { stateNameDepth = depth + 1 }
  ret <- withName $ Ident $ "$gen" ++ show depth
  modify $ \s -> s { stateNameDepth = depth }
  return ret

-- | Resolve operators to only built-in (for now) functions.
resolveOpToVar :: QOp -> Compile Exp
resolveOpToVar op =
  case getOp op of
    UnQual (Symbol symbol)
      | symbol == "*"   -> return (Var (fayBuiltin "mult"))
      | symbol == "+"   -> return (Var (fayBuiltin "add"))
      | symbol == "-"   -> return (Var (fayBuiltin "sub"))
      | symbol == "/"   -> return (Var (fayBuiltin "div"))
      | symbol == "=="  -> return (Var (fayBuiltin "eq"))
      | symbol == "/="  -> return (Var (fayBuiltin "neq"))
      | symbol == ">"   -> return (Var (fayBuiltin "gt"))
      | symbol == "<"   -> return (Var (fayBuiltin "lt"))
      | symbol == ">="  -> return (Var (fayBuiltin "gte"))
      | symbol == "<="  -> return (Var (fayBuiltin "lte"))
      | symbol == "&&"  -> return (Var (fayBuiltin "and"))
      | symbol == "||"  -> return (Var (fayBuiltin "or"))
      | otherwise       -> return (Var (fromString symbol))
    n@(UnQual Ident{})  -> return (Var n)
    Special Cons        -> return (Var (fayBuiltin "cons"))
    _                   -> throwError (UnsupportedOperator op)

  where getOp (QVarOp o) = o
        getOp (QConOp o) = o
