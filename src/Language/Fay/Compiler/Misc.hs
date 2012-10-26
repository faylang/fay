{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans  #-}

-- | Miscellaneous functions used throughout the compiler.

module Language.Fay.Compiler.Misc where

import Language.Fay.Print          (jsEncodeName)
import Language.Fay.Types

import Control.Monad.Error
import Control.Monad.State
import Data.List
import Data.String
import Language.Haskell.Exts (ParseResult(..))
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)

instance IsString Name where fromString = Ident

-- | Extra the string from an ident.
unname :: Name -> String
unname (Ident str) = str
unname _ = error "Expected ident from uname." -- FIXME:

-- | Make an identifier from the built-in HJ module.
hjIdent :: String -> QName
hjIdent = Qual (ModuleName "Fay") . Ident

-- | Wrap an expression in a thunk.
thunk :: JsExp -> JsExp
-- thunk exp = JsNew (hjIdent "Thunk") [JsFun [] [] (Just exp)]
thunk expr =
  case expr of
    -- JS constants don't need to be in thunks, they're already strict.
    JsLit{} -> expr
    JsName "true" -> expr
    JsName "false" -> expr
    -- Functions (e.g. lets) used for introducing a new lexical scope
    -- aren't necessary inside a thunk. This is a simple aesthetic
    -- optimization.
    JsApp fun@JsFun{} [] -> JsNew ":thunk" [fun]
    -- Otherwise make a regular thunk.
    _ -> JsNew ":thunk" [JsFun [] [] (Just expr)]

-- | Wrap an expression in a thunk.
stmtsThunk :: [JsStmt] -> JsExp
stmtsThunk stmts = JsNew ":thunk" [JsFun [] stmts Nothing]

-- | Generate unique names.
uniqueNames :: [JsParam]
uniqueNames = map (fromString . ("$_" ++))
            $ map return "abcxyz" ++
              zipWith (:) (cycle "v")
                          (map show [1 :: Integer ..])

-- | Make a top-level binding.
bindToplevel :: SrcLoc -> Bool -> QName -> JsExp -> Compile JsStmt
bindToplevel srcloc toplevel name expr = do
  exportAll <- gets stateExportAll
  when (toplevel && exportAll) $ emitExport (EVar name)
  return (JsMappedVar srcloc name expr)

-- | Emit exported names.
emitExport :: ExportSpec -> Compile ()
emitExport spec =
  case spec of
    EVar (UnQual name) -> modify $ \s -> s { stateExports = name : stateExports s }
    EVar _             -> error "Emitted a qualifed export, not supported."
    _ -> throwError (UnsupportedExportSpec spec)

-- | Force an expression in a thunk.
force :: JsExp -> JsExp
force expr
  | isConstant expr = expr
  | otherwise = JsApp (JsName "_") [expr]

-- | Is a JS expression a literal (constant)?
isConstant :: JsExp -> Bool
isConstant JsLit{} = True
isConstant _       = False

-- | Extract the string from a qname.
qname :: QName -> String
qname (UnQual (Ident str)) = str
qname (UnQual (Symbol sym)) = jsEncodeName sym
qname i = error $ "qname: Expected unqualified ident, found: " ++ show i -- FIXME:

-- | Make a constructor name.
constructorName :: QName -> QName
constructorName = fromString . ("$_" ++) . qname

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
withScopedTmpName :: (JsName -> Compile a) -> Compile a
withScopedTmpName withName = do
  depth <- gets stateNameDepth
  modify $ \s -> s { stateNameDepth = depth + 1 }
  ret <- withName $ fromString $ "$_tmp" ++ show depth
  modify $ \s -> s { stateNameDepth = depth }
  return ret

-- | Resolve operators to only built-in (for now) functions.
resolveOpToVar :: QOp -> Compile Exp
resolveOpToVar op =
  case getOp op of
    UnQual (Symbol symbol)
      | symbol == "*"   -> return (Var (hjIdent "mult"))
      | symbol == "+"   -> return (Var (hjIdent "add"))
      | symbol == "-"   -> return (Var (hjIdent "sub"))
      | symbol == "/"   -> return (Var (hjIdent "div"))
      | symbol == "=="  -> return (Var (hjIdent "eq"))
      | symbol == "/="  -> return (Var (hjIdent "neq"))
      | symbol == ">"   -> return (Var (hjIdent "gt"))
      | symbol == "<"   -> return (Var (hjIdent "lt"))
      | symbol == ">="  -> return (Var (hjIdent "gte"))
      | symbol == "<="  -> return (Var (hjIdent "lte"))
      | symbol == "&&"  -> return (Var (hjIdent "and"))
      | symbol == "||"  -> return (Var (hjIdent "or"))
      | otherwise       -> return (Var (fromString symbol))
    n@(UnQual Ident{})  -> return (Var n)
    Special Cons        -> return (Var (hjIdent "cons"))
    _                   -> throwError (UnsupportedOperator op)

  where getOp (QVarOp o) = o
        getOp (QConOp o) = o
