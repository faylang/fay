{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans  #-}

-- | Miscellaneous functions used throughout the compiler.

module Language.Fay.Compiler.Misc where

import           Language.Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.String
import           Language.Haskell.Exts        (ParseResult(..))
import           Language.Haskell.Exts.Syntax
import           Prelude                      hiding (exp)
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
resolveName (UnQual name) = do
--  let echo = io . putStrLn
--  echo $ "Resolving name " ++ prettyPrint name
  names <- gets stateScope
--  echo $ "Names are: " ++ show names
  case M.lookup name names of
    -- Unqualified and not imported? Current module.
    Nothing -> qualify name
    Just scopes -> case find localBinding scopes of
      Just ScopeBinding -> return (UnQual name)
      _ ->
        case find simpleImport scopes of
          Just (ScopeImported modulename replacement) -> return (Qual modulename (fromMaybe name replacement))
          _ -> case find asImport scopes of
            Just (ScopeImportedAs _ modulename _) -> return (Qual modulename name)
            _ -> throwError $ UnableResolveUnqualified name

  where asImport ScopeImportedAs{} = True
        asImport _ = False

        localBinding ScopeBinding = True
        localBinding _ = False

resolveName (Qual modulename name) = do
  names <- gets stateScope
  case M.lookup name names of
    -- Qualified and not imported? It's correct, leave it as-is.
    Nothing -> return (Qual modulename name)
    Just scopes -> case find simpleImport scopes of
      Just (ScopeImported _ replacement) -> return (Qual modulename (fromMaybe name replacement))
      _ -> case find asMatch scopes of
        Just (ScopeImported realname replacement) -> return (Qual realname (fromMaybe name replacement))
        _ -> throwError $ UnableResolveQualified (Qual modulename name)

  where asMatch i = case i of
          ScopeImported{} -> True
          ScopeImportedAs _ _ qmodulename -> qmodulename == moduleToName modulename
          ScopeBinding -> False
          where moduleToName (ModuleName n) = Ident n

-- | Do have have a simple "import X" import on our hands?
simpleImport :: NameScope -> Bool
simpleImport ScopeImported{} = True
simpleImport _ = False

-- | Qualify a name for the current module.
qualify :: Name -> Compile QName
qualify name = do
  modulename <- gets stateModuleName
  return (Qual modulename name)

-- | Make a top-level binding.
bindToplevel :: SrcLoc -> Bool -> Name -> JsExp -> Compile JsStmt
bindToplevel srcloc toplevel name expr = do
  qname <- (if toplevel then qualify else return . UnQual) name
  exportAll <- gets stateExportAll
  -- If exportAll is set this declaration has not been added to stateExports yet.
  when (toplevel && exportAll) $ emitExport (EVar qname)
  return (JsMappedVar srcloc (JsNameVar qname) expr)

-- | Create a temporary scope and discard it after the given computation.
withScope :: Compile a -> Compile a
withScope m = do
  scope <- gets stateScope
  value <- m
  modify $ \s -> s { stateScope = scope }
  return value

-- | Run a compiler and just get the scope information.
generateScope :: Compile a -> Compile ()
generateScope m = do
  st <- get
  _ <- m
  scope <- gets stateScope
  put st { stateScope = scope }

-- | Bind a variable in the current scope.
bindVar :: Name -> Compile ()
bindVar name = do
  modify $ \s -> s { stateScope = M.insertWith (++) name [ScopeBinding] (stateScope s) }

-- | Emit exported names.
emitExport :: ExportSpec -> Compile ()
emitExport spec =
  case spec of
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
