{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans  #-}

-- | Miscellaneous functions used throughout the compiler.

module Language.Fay.Compiler.Misc where

import qualified Language.Fay.ModuleScope     as ModuleScope
import           Language.Fay.Types

import           System.Process                  (readProcess)
import           Text.ParserCombinators.ReadP    (readP_to_S)
import           Data.Version                    (parseVersion)
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import           Data.List
import qualified Data.Set                     as S
import           Data.Maybe
import           Data.String
import           Language.Haskell.Exts        (ParseResult(..))
import           Language.Haskell.Exts.Syntax
import           Prelude                      hiding (exp, mod)
import           System.IO

-- | Extra the string from an ident.
unname :: Name -> String
unname (Ident str) = str
unname _ = error "Expected ident from uname." -- FIXME:

-- | Make an identifier from the built-in HJ module.
fayBuiltin :: String -> QName
fayBuiltin = Qual (ModuleName "Fay$") . Ident

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

-- | Resolve a given maybe-qualified name to a fully qualifed name.
resolveName :: QName -> Compile QName
resolveName special@Special{} = return special
resolveName q@Qual{} = do
  env <- gets stateModuleScope
  maybe (throwError $ UnableResolveQualified q) return (ModuleScope.resolveName q env)
resolveName u@(UnQual name) = do
  names <- gets stateLocalScope
  env <- gets stateModuleScope
  if S.member name names
    then return (UnQual name)
    else maybe (qualify name) return (ModuleScope.resolveName u env)

-- | Qualify a name for the current module.
qualify :: Name -> Compile QName
qualify name = do
  modulename <- gets stateModuleName
  return (Qual modulename name)

-- | Make a top-level binding.
bindToplevel :: SrcLoc -> Bool -> Name -> JsExp -> Compile JsStmt
bindToplevel srcloc toplevel name expr = do
  qname <- (if toplevel then qualify else return . UnQual) name
  -- If exportAll is set this declaration has not been added to stateExports yet.
  return (JsMappedVar srcloc (JsNameVar qname) expr)

-- | Create a temporary environment and discard it after the given computation.
withModuleScope :: Compile a -> Compile a
withModuleScope m = do
  scope <- gets stateModuleScope
  value <- m
  modify $ \s -> s { stateModuleScope = scope }
  return value

-- | Create a temporary scope and discard it after the given computation.
withScope :: Compile a -> Compile a
withScope m = do
  scope <- gets stateLocalScope
  value <- m
  modify $ \s -> s { stateLocalScope = scope }
  return value

-- | Run a compiler and just get the scope information.
generateScope :: Compile a -> Compile ()
generateScope m = do
  st <- get
  _ <- m
  scope <- gets stateLocalScope
  put st { stateLocalScope = scope }

-- | Bind a variable in the current scope.
bindVar :: Name -> Compile ()
bindVar name = do
  modify $ \s -> s { stateLocalScope = S.insert name (stateLocalScope s) }

-- | Emit exported names.
emitExport :: ExportSpec -> Compile ()
emitExport spec = case spec of
  EVar (UnQual name) -> emitVar (UnQual name)
  EVar name@Qual{} -> modify $ \s -> s { stateExports = name : stateExports s }
  EThingAll (UnQual name) -> do
    emitVar (UnQual name)
    r <- lookup (UnQual name) <$> gets stateRecords
    maybe (return ()) (mapM_ emitVar) r
  EThingWith (UnQual name) ns -> do
    emitVar (UnQual name)
    mapM_ emitCName ns
  EAbs _ -> return () -- Type only, skip
  _ -> do
    name <- gets stateModuleName
    unless (name == "Language.Fay.Stdlib") $
      throwError (UnsupportedExportSpec spec)
 where
   emitVar n = resolveName n >>= emitExport . EVar
   emitCName (VarName n) = emitVar (UnQual n)
   emitCName (ConName n) = emitVar (UnQual n)

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
parseResult die ok result = case result of
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

-- | Print out a compiler warning.
warn :: String -> Compile ()
warn "" = return ()
warn w = do
  shouldWarn <- configWarn <$> gets stateConfig
  when shouldWarn . liftIO . hPutStrLn stderr $ "Warning: " ++ w

-- | Pretty print a source location.
printSrcLoc :: SrcLoc -> String
printSrcLoc SrcLoc{..} = srcFilename ++ ":" ++ show srcLine ++ ":" ++ show srcColumn

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p l = return . not . null =<< filterM p l

typeToRecs :: QName -> Compile [QName]
typeToRecs typ = fromMaybe [] . lookup typ <$> gets stateRecordTypes

typeToFields :: QName -> Compile [QName]
typeToFields typ = do
  allrecs <- gets stateRecords
  typerecs <- typeToRecs typ
  return . concatMap snd . filter ((`elem` typerecs) . fst) $ allrecs

-- | Get the flag used for GHC, this differs between GHC-7.6.0 and
-- GHC-everything-else so we need to specially test for that. It's
-- lame, but that's random flag name changes for you.
getGhcPackageDbFlag :: IO String
getGhcPackageDbFlag = do
  s <- readProcess "ghc" ["--version"] ""
  return $
      case (mapMaybe readVersion $ words s, readVersion "7.6.0") of
          (v:_, Just min') | v > min' -> "-package-db"
          _ -> "-package-conf"
  where
    readVersion = listToMaybe . filter (null . snd) . readP_to_S parseVersion
