{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans  #-}

-- | Miscellaneous functions used throughout the compiler.

module Fay.Compiler.Misc where

import qualified Fay.Compiler.ModuleScope        as ModuleScope
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.RWS
import           Data.List
import           Data.Maybe
import qualified Data.Set                        as S
import           Data.String
import           Data.Version                    (parseVersion)
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Fixity
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax
import           Prelude                         hiding (exp, mod)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process                  (readProcess)
import           Text.ParserCombinators.ReadP    (readP_to_S)

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

lookupNewtypeConst :: QName -> Compile (Maybe (Maybe QName,Type))
lookupNewtypeConst name = do
  newtypes <- gets stateNewtypes
  case find (\(cname,_,_) -> cname == name) newtypes of
    Nothing -> return Nothing
    Just (_,dname,ty) -> return $ Just (dname,ty)

lookupNewtypeDest :: QName -> Compile (Maybe (QName,Type))
lookupNewtypeDest name = do
  newtypes <- gets stateNewtypes
  case find (\(_,dname,_) -> dname == Just name) newtypes of
    Nothing -> return Nothing
    Just (cname,_,ty) -> return $ Just (cname,ty)

-- | Qualify a name for the current module.
qualify :: Name -> Compile QName
qualify name = do
  modulename <- gets stateModuleName
  return (Qual modulename name)

-- | Qualify a QName for the current module if unqualified.
qualifyQName :: QName -> Compile QName
qualifyQName (UnQual name) = qualify name
qualifyQName n             = return n

-- | Make a top-level binding.
bindToplevel :: SrcLoc -> Bool -> Name -> JsExp -> Compile JsStmt
bindToplevel srcloc toplevel name expr = do
  qname <- (if toplevel then qualify else return . UnQual) name
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
  EVar (UnQual n) -> emitVar n
  EVar q@Qual{} -> modify $ addCurrentExport q
  EThingAll (UnQual name) -> do
    emitVar name
    r <- lookup (UnQual name) <$> gets stateRecords
    maybe (return ()) (mapM_ (emitVar . unQName)) r
  EThingWith (UnQual name) ns -> do
    emitVar name
    mapM_ emitCName ns
  EAbs _ -> return () -- Type only, skip
  EModuleContents mod ->
    mapM_ (emitExport . EVar) =<< ModuleScope.moduleLocals mod <$> gets stateModuleScope

  -- Skip qualified exports for type exports in fay-base since
  -- qualified imports are not supported yet an error will be thrown
  -- on the import so hopefully this won't be confusing.
  EThingAll (Qual _ _) -> return ()
  e -> do
    liftIO (print e)
    throwError $ UnsupportedExportSpec e
 where
   emitVar = return . UnQual >=> resolveName >=> emitExport . EVar
   emitCName (VarName n) = emitVar n
   emitCName (ConName n) = emitVar n
   unQName (UnQual u) = u
   unQName _ = error "unQName Qual or Special -- should never happen"

-- | Force an expression in a thunk.
force :: JsExp -> JsExp
force expr
  | isConstant expr = expr
  | otherwise = JsApp (JsName JsForce) [expr]

-- | Is a JS expression a literal (constant)?
isConstant :: JsExp -> Bool
isConstant JsLit{} = True
isConstant _       = False

-- | Deconstruct a parse result (a la maybe, foldr, either).
parseResult :: ((SrcLoc,String) -> b) -> (a -> b) -> ParseResult a -> b
parseResult die ok result = case result of
  ParseOk a -> ok a
  ParseFailed srcloc msg -> die (srcloc,msg)

-- | Get a config option.
config :: (CompileConfig -> a) -> Compile a
config f = asks (f . readerConfig)

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
  shouldWarn <- config configWarn
  when shouldWarn . liftIO . hPutStrLn stderr $ "Warning: " ++ w

-- | Pretty print a source location.
printSrcLoc :: SrcLoc -> String
printSrcLoc SrcLoc{..} = srcFilename ++ ":" ++ show srcLine ++ ":" ++ show srcColumn

-- | Lookup the record for a given type name.
typeToRecs :: QName -> Compile [QName]
typeToRecs typ = fromMaybe [] . lookup typ <$> gets stateRecordTypes

-- | Get the fields for a given type.
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

-- | Find an import's filepath and contents from its module name.
findImport :: [FilePath] -> ModuleName -> Compile (FilePath,String)
findImport alldirs mname = go alldirs mname where
  go (dir:dirs) name = do
    exists <- io (doesFileExist path)
    if exists
      then fmap (path,) (fmap stdlibHack (io (readFile path)))
      else go dirs name
    where
      path = dir </> replace '.' '/' (prettyPrint name) ++ ".hs"
      replace c r = map (\x -> if x == c then r else x)
  go [] name =
    throwError $ Couldn'tFindImport name alldirs

  stdlibHack
    | mname == ModuleName "Language.Fay.Stdlib" = \s -> s ++ "\n\ndata Maybe a = Just a | Nothing"
    | mname == ModuleName "Language.Fay.FFI"    = const "module Language.Fay.FFI where\n\ndata Nullable a = Nullable a | Null\n\ndata Defined a = Defined a | Undefined"
    | otherwise = id

-- | Convert a GADT to a normal data type.
convertGADT :: GadtDecl -> QualConDecl
convertGADT d =
  case d of
    GadtDecl srcloc name typ -> QualConDecl srcloc tyvars context
                                            (ConDecl name (convertFunc typ))
  where tyvars = []
        context = []
        convertFunc (TyCon _) = []
        convertFunc (TyFun x xs) = UnBangedTy x : convertFunc xs
        convertFunc (TyParen x) = convertFunc x
        convertFunc _ = []

-- | Run the compiler.
runCompile :: CompileReader -> CompileState
           -> Compile a
           -> IO (Either CompileError (a,CompileState,CompileWriter))
runCompile reader' state' m = runErrorT (runRWST (unCompile m) reader' state')

-- | Parse some Fay code.
parseFay :: Parseable ast => FilePath -> String -> ParseResult ast
parseFay filepath = parseWithMode parseMode { parseFilename = filepath } . applyCPP

-- | Apply incredibly simplistic CPP handling. It only recognizes the following:
--
-- > #if FAY
-- > #ifdef FAY
-- > #ifndef FAY
-- > #else
-- > #endif
--
-- Note that this implementation replaces all removed lines with blanks, so
-- that line numbers remain accurate.
applyCPP :: String -> String
applyCPP =
    unlines . loop NoCPP . lines
  where
    loop _ [] = []
    loop state' ("#if FAY":rest) = "" : loop (CPPIf True state') rest
    loop state' ("#ifdef FAY":rest) = "" : loop (CPPIf True state') rest
    loop state' ("#ifndef FAY":rest) = "" : loop (CPPIf False state') rest
    loop (CPPIf b oldState') ("#else":rest) = "" : loop (CPPElse (not b) oldState') rest
    loop (CPPIf _ oldState') ("#endif":rest) = "" : loop oldState' rest
    loop (CPPElse _ oldState') ("#endif":rest) = "" : loop oldState' rest
    loop state' (x:rest) = (if toInclude state' then x else "") : loop state' rest

    toInclude NoCPP = True
    toInclude (CPPIf x state') = x && toInclude state'
    toInclude (CPPElse x state') = x && toInclude state'

-- | The CPP's parsing state.
data CPPState = NoCPP
              | CPPIf Bool CPPState
              | CPPElse Bool CPPState

-- | The parse mode for Fay.
parseMode :: ParseMode
parseMode = defaultParseMode
  { extensions = [GADTs
                 ,StandaloneDeriving
                 ,PackageImports
                 ,EmptyDataDecls
                 ,TypeOperators
                 ,RecordWildCards
                 ,NamedFieldPuns]
  , fixities = Just (preludeFixities ++ baseFixities)
  }
