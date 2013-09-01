{-# OPTIONS -Wall -fno-warn-orphans  #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Miscellaneous functions used throughout the compiler.

module Fay.Compiler.Misc where

import Distribution.HaskellSuite.Modules
import           Fay.Control.Monad.IO
import qualified Fay.Compiler.ModuleScope        as ModuleScope
import           Fay.Types
import qualified Fay.Exts as F
import qualified Fay.Exts.NoAnnotation as N
import qualified Fay.Exts.Scoped as S
import Fay.Exts.NoAnnotation (unAnn)
import Data.Char (isAlpha)
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.RWS
import           Data.List
import           Data.Maybe
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Data.String
import           Data.Version                    (parseVersion)
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Fixity
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Annotated hiding (name)
import           Prelude                         hiding (exp, mod)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process                  (readProcess)
import           Text.ParserCombinators.ReadP    (readP_to_S)
import Language.Haskell.Names

-- | Make an identifier from the built-in HJ module.
fayBuiltin :: String -> N.QName
fayBuiltin = Qual () (ModuleName () "Fay$") . Ident ()

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
    _ -> JsNew JsThunk [JsFun Nothing [] [] (Just expr)]

-- | Wrap an expression in a thunk.
stmtsThunk :: [JsStmt] -> JsExp
stmtsThunk stmts = JsNew JsThunk [JsFun Nothing [] stmts Nothing]

-- | Generate unique names.
uniqueNames :: [JsName]
uniqueNames = map JsParam [1::Integer ..]

tryResolveName :: Show l => QName (Scoped l) -> Compile (Maybe N.QName)
tryResolveName u = do
  r <- tryResolveName' u
  -- io $ print ("tryResolveName"::String,unAnn u ,fmap unAnn r)
  return r

-- | Resolve a given maybe-qualified name to a fully qualifed name.
-- TODO purify
tryResolveName' :: Show l => QName (Scoped l) -> Compile (Maybe N.QName)
tryResolveName' special@Special{} = return . Just $ unAnn special
tryResolveName' s@(UnQual _ (Ident _ n))
  | "$gen" `isPrefixOf` n = return $ Just $ unAnn s
tryResolveName' (unAnn -> Qual () (ModuleName () "$Prelude") n) =
  return $ Just $ Qual () (ModuleName () "Prelude") n
tryResolveName' q@(Qual _ (ModuleName _ "Fay$") _) = return $ Just $ unAnn q
tryResolveName' (Qual (Scoped ni _) _ _) = case ni of
    GlobalValue nx -> return $ replaceWithBuiltIns $ gname2Qname $ origGName $ sv_origName nx
    LocalValue _ -> return $ Nothing
    GlobalType _ -> return $ Nothing
    TypeVar _ -> return $ Nothing
    Binder -> return $ Nothing
    Import _ -> return $ Nothing
    ImportPart _ -> return $ Nothing
    Export _ -> return $ Nothing
    None -> return $ Nothing
    ScopeError _ -> return $ Nothing
tryResolveName' q@(UnQual (Scoped ni _) name) = case ni of
    GlobalValue nx -> return $ replaceWithBuiltIns $ gname2Qname $ origGName $ sv_origName nx
    LocalValue _ -> return $ Just $ UnQual () (unAnn name)
    GlobalType _ -> return $ Nothing
    TypeVar _ -> return $ Nothing
    Binder -> return $ Nothing
    Import _ -> return $ Nothing
    ImportPart _ -> return $ Nothing
    Export _ -> return $ Nothing
    None -> return $ Nothing
    ScopeError _ -> return $ ModuleScope.resolvePrimOp q

gname2Qname :: GName -> N.QName
gname2Qname g = case g of
  GName "" s -> UnQual () $ mkName s
  GName m  s -> Qual () (ModuleName () m) $ mkName s
  where
    mkName s@(x:_)
      | isAlpha x || x == '_' = Ident () s
      | otherwise = Symbol () s
    mkName "" = error "mkName \"\""

replaceWithBuiltIns :: N.QName -> Maybe N.QName
replaceWithBuiltIns n = ModuleScope.findPrimOp n <|> return n

-- | Resolve a given maybe-qualified name to a fully qualifed name.
-- Use this when a resolution failure is a bug.
unsafeResolveName :: S.QName -> Compile N.QName
unsafeResolveName q = maybe (throwError $ UnableResolveQualified (unAnn q)) return =<< tryResolveName q

-- | Resolve a newtype constructor.
lookupNewtypeConst :: S.QName -> Compile (Maybe (Maybe N.QName,N.Type))
lookupNewtypeConst n = do
  mName <- tryResolveName n
  case mName of
    Nothing -> return Nothing
    Just name -> do
      newtypes <- gets stateNewtypes
      case find (\(cname,_,_) -> cname == name) newtypes of
        Nothing -> return Nothing
        Just (_,dname,ty) -> return $ Just (dname,ty)

-- | Resolve a newtype destructor.
lookupNewtypeDest :: S.QName -> Compile (Maybe (N.QName,N.Type))
lookupNewtypeDest n = do
  mName <- tryResolveName n
  newtypes <- gets stateNewtypes
  case find (\(_,dname,_) -> dname == mName) newtypes of
    Nothing -> return Nothing
    Just (cname,_,ty) -> return $ Just (cname,ty)

-- | Qualify a name for the current module.
qualify :: Name a -> Compile (N.QName)
qualify (Ident _ name) = do
  modulename <- gets stateModuleName
  return (Qual () modulename (Ident () name))
qualify (Symbol _ name) = do
  modulename <- gets stateModuleName
  return (Qual () modulename (Symbol () name))

-- | Qualify a QName for the current module if unqualified.
qualifyQName :: QName a -> Compile N.QName
qualifyQName (UnQual _ name) = qualify name
qualifyQName (unAnn -> n)    = return n

-- | Make a top-level binding.
bindToplevel :: Bool -> Name a -> JsExp -> Compile JsStmt
bindToplevel toplevel (unAnn -> name) expr =
  if toplevel
    then do
      mod <- gets stateModuleName
      return $ JsSetQName (Qual () mod name) expr
    else return $ JsVar (JsNameVar $ UnQual () name) expr


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
bindVar :: Name a -> Compile ()
bindVar (unAnn -> name) =
  modify $ \s -> s { stateLocalScope = S.insert name (stateLocalScope s) }

-- | Emit exported names.
emitExport :: ExportSpec a -> Compile ()
emitExport _ = return ()

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
parseResult :: ((F.SrcLoc,String) -> b) -> (a -> b) -> ParseResult a -> b
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
isWildCardAlt :: S.Alt -> Bool
isWildCardAlt (Alt _ pat _ _) = isWildCardPat pat

-- | Is a pattern a wildcard?
isWildCardPat :: S.Pat -> Bool
isWildCardPat PWildCard{} = True
isWildCardPat PVar{}      = True
isWildCardPat _           = False

-- | Return formatter string if expression is a FFI call.
ffiExp :: Exp a -> Maybe String
ffiExp (App _ (Var _ (UnQual _ (Ident _ "ffi"))) (Lit _ (String _ formatstr _))) = Just formatstr
ffiExp _ = Nothing

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
withScopedTmpName :: (S.Name -> Compile a) -> Compile a
withScopedTmpName withName = do
  depth <- gets stateNameDepth
  modify $ \s -> s { stateNameDepth = depth + 1 }
  ret <- withName $ Ident S.noI $ "$gen" ++ show depth
  modify $ \s -> s { stateNameDepth = depth }
  return ret

-- | Print out a compiler warning.
warn :: String -> Compile ()
warn "" = return ()
warn w = do
  shouldWarn <- config configWarn
  when shouldWarn . io . hPutStrLn stderr $ "Warning: " ++ w

-- | Pretty print a source location.
printSrcLoc :: S.SrcLoc -> String
printSrcLoc SrcLoc{..} = srcFilename ++ ":" ++ show srcLine ++ ":" ++ show srcColumn

printSrcSpanInfo :: SrcSpanInfo -> String
printSrcSpanInfo (SrcSpanInfo a b) = concat $ printSrcSpan a : map printSrcSpan b

printSrcSpan :: SrcSpan -> String
printSrcSpan SrcSpan{..} = srcSpanFilename ++ ": (" ++ show srcSpanStartLine ++ "," ++ show srcSpanStartColumn ++ ")-(" ++ show srcSpanEndLine ++ "," ++ show srcSpanEndColumn


-- | Lookup the record for a given type name.
typeToRecs :: QName a -> Compile [N.QName]
typeToRecs (unAnn -> typ) = fromMaybe [] . lookup typ <$> gets stateRecordTypes

-- | Get the fields for a given type.
typeToFields :: QName a -> Compile [N.QName]
typeToFields (unAnn -> typ) = do
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
findImport :: [FilePath] -> ModuleName a -> Compile (FilePath,String)
findImport alldirs (unAnn -> mname) = go alldirs mname where
  go :: [FilePath] -> ModuleName a -> Compile (FilePath,String)
  go _ (ModuleName _ "Fay.Types") = return ("Fay/Types.hs", "newtype Fay a = Fay (Identity a)\n\nnewtype Identity a = Identity a")
  go (dir:dirs) name = do
    exists <- io (doesFileExist path)
    if exists
      then (path,) . stdlibHack <$> io (readFile path)
      else go dirs name
    where
      path = dir </> replace '.' '/' (prettyPrint name) ++ ".hs"
      replace c r = map (\x -> if x == c then r else x)
  go [] name =
    throwError $ Couldn'tFindImport (unAnn name) alldirs

  stdlibHack = case mname of
    ModuleName _ "Fay.FFI" -> const "module Fay.FFI where\n\ndata Nullable a = Nullable a | Null\n\ndata Defined a = Defined a | Undefined"
    _ -> id

-- | Run the compiler.
topRunCompile :: CompileReader -> CompileState
           -> Compile a
           -> IO (Either CompileError (a,CompileState,CompileWriter))
topRunCompile reader' state' m = fst <$> runModuleT (runErrorT (runRWST (unCompile m) reader' state')) [] "fay" (\_fp -> return undefined) M.empty


runCompile :: CompileReader -> CompileState -> Compile a
           -> Compile2 a
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
  { extensions = map EnableExtension
                   [GADTs
                   ,ExistentialQuantification
                   ,StandaloneDeriving
                   ,PackageImports
                   ,EmptyDataDecls
                   ,TypeOperators
                   ,RecordWildCards
                   ,NamedFieldPuns
                   ,FlexibleContexts
                   ,FlexibleInstances
                   ,KindSignatures]
  , fixities = Just (preludeFixities ++ baseFixities)
  }
