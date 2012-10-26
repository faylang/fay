{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | The Haskell→Javascript compiler.

module Language.Fay
  (compile
  ,runCompile
  ,compileViaStr
  ,compileForDocs
  ,compileToAst
  ,compileFromStr
  ,compileModule
  ,compileExp
  ,compileDecl
  ,printCompile
  ,printTestCompile
  ,compileToplevelModule
  ,prettyPrintString)
  where

import           Language.Fay.Print          (jsEncodeName, printJSString)
import           Language.Fay.Types
import           System.Process.Extra

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Data.Char
import           Data.Default                (def)
import           Data.List
import           Data.Maybe
import           Data.String
import qualified Language.ECMAScript3.Parser as JS
import           Language.Haskell.Exts

import           Safe
import           System.Directory            (doesFileExist, findExecutable)
import           System.Exit
import           System.FilePath             ((</>))
import           System.IO
import           System.Process

--------------------------------------------------------------------------------
-- Top level entry points

-- | Compile something that compiles to something else.
compile :: CompilesTo from to => CompileConfig -> from -> IO (Either CompileError (to,CompileState))
compile config = runCompile (defaultCompileState config) . compileTo

-- | Run the compiler.
runCompile :: CompileState -> Compile a -> IO (Either CompileError (a,CompileState))
runCompile state m = runErrorT (runStateT (unCompile m) state) where

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (String,CompileState))
compileViaStr config with from =
  runCompile (defaultCompileState config)
             (parseResult (throwError . uncurry ParseError)
                          (fmap printJSString . with)
                          (parseFay from))

-- | Compile a Haskell source string to a JavaScript source string.
compileToAst :: (Show from,Show to,CompilesTo from to)
              => CompileState
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (to,CompileState))
compileToAst state with from =
  runCompile state
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay from))

-- | Compile from a string.
compileFromStr :: (Parseable a, MonadError CompileError m) => (a -> m a1) -> String -> m a1
compileFromStr with from =
  parseResult (throwError . uncurry ParseError)
              with
              (parseFay from)

-- | Parse some Fay code.
parseFay :: Parseable ast => String -> ParseResult ast
parseFay = parseWithMode parseMode

-- | The parse mode for Fay.
parseMode :: ParseMode
parseMode = defaultParseMode { extensions =
  [GADTs,StandaloneDeriving,EmptyDataDecls,TypeOperators,RecordWildCards,NamedFieldPuns] }

-- | Compile the given input and print the output out prettily.
printCompile :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO ()
printCompile config with from = do
  result <- compileViaStr config with from
  case result of
    Left err -> print err
    Right (ok,_) -> prettyPrintString ok >>= putStr

-- | Compile a String of Fay and print it as beautified JavaScript.
printTestCompile :: String -> IO ()
printTestCompile = printCompile def { configWarn = False } compileModule

-- | Compile the given Fay code for the documentation. This is
-- specialised because the documentation isn't really “real”
-- compilation.
compileForDocs :: Module -> Compile [JsStmt]
compileForDocs mod = do
  initialPass mod
  compileModule mod

-- | Compile the top-level Fay module.
compileToplevelModule :: Module -> Compile [JsStmt]
compileToplevelModule mod@(Module _ (ModuleName modulename) _ _ _ _ _)  = do
  cfg <- gets stateConfig
  when (configTypecheck cfg) $
    typecheck (configDirectoryIncludes cfg) [] (configWall cfg) $
      fromMaybe modulename $ configFilePath cfg
  initialPass mod
  modify $ \s -> s { stateImported = stateImported (defaultCompileState def) }
  stmts <- compileModule mod
  fay2js <- gets (fayToJsDispatcher . stateFayToJs)
  js2fay <- gets (jsToFayDispatcher . stateJsToFay)
  return (stmts ++ [fay2js,js2fay])

--------------------------------------------------------------------------------
-- Initial pass-through collecting record definitions

initialPass :: Module -> Compile ()
initialPass (Module _ _ _ Nothing _ imports decls) = do
  mapM_ initialPass_import imports
  mapM_ (initialPass_decl True) decls

initialPass mod = throwError (UnsupportedModuleSyntax mod)

initialPass_import :: ImportDecl -> Compile ()
initialPass_import (ImportDecl _ (ModuleName name) False _ Nothing Nothing Nothing) = do
  void $ unlessImported name $ do
    dirs <- configDirectoryIncludes <$> gets stateConfig
    contents <- io (findImport dirs name)
    cs <- gets id
    result <- liftIO $ initialPass_records cs initialPass contents
    case result of
      Right ((),state) -> do
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords = stateRecords state
                         , stateImported = stateImported state
                         }
      Left err -> throwError err
    return []

initialPass_import i =
  error $ "Initial pass: Import syntax not supported. " ++
        "The compiler writer was too lazy to support that.\n" ++
        "It was: " ++ show i

initialPass_records :: (Show from,Parseable from)
              => CompileState
              -> (from -> Compile ())
              -> String
              -> IO (Either CompileError ((),CompileState))
initialPass_records compileState with from =
  runCompile compileState
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay from))

initialPass_decl :: Bool -> Decl -> Compile ()
initialPass_decl toplevel decl =
  case decl of
    DataDecl _ DataType _ _ _ constructors _ -> initialPass_dataDecl toplevel decl constructors
    GDataDecl _ DataType _l _i _v _n decls _ -> initialPass_dataDecl toplevel decl (map convertGADT decls)
    _ -> return ()

-- | Collect record definitions and store record name and field names.
-- A ConDecl will have fields named slot1..slotN
initialPass_dataDecl :: Bool -> Decl -> [QualConDecl] -> Compile ()
initialPass_dataDecl _ _decl constructors =
  forM_ constructors $ \(QualConDecl _ _ _ condecl) ->
    case condecl of
      ConDecl (UnQual -> name) types  -> do
        let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
        addRecordState name fields
      InfixConDecl _t1 (UnQual -> name) _t2 ->
        addRecordState name ["slot1", "slot2"]
      RecDecl (UnQual -> name) fields' -> do
        let fields = concatMap fst fields'
        addRecordState name fields

  where
    addRecordState :: QName -> [Name] -> Compile ()
    addRecordState name fields = modify $ \s -> s { stateRecords = (Ident (qname name), fields) : stateRecords s }

--------------------------------------------------------------------------------
-- Typechecking

typecheck :: [FilePath] -> [String] -> Bool -> String -> Compile ()
typecheck includeDirs ghcFlags wall fp = do
  res <- liftIO $ readAllFromProcess' "ghc" (
    ["-fno-code", "-package fay", fp] ++ map ("-i" ++) includeDirs ++ ghcFlags ++ wallF) ""
  either error (warn . fst) res
  where
    wallF | wall = ["-Wall"]
          | otherwise = []

--------------------------------------------------------------------------------
-- Compilers


-- | Compile Haskell module.
compileModule :: Module -> Compile [JsStmt]
compileModule (Module _ modulename pragmas Nothing exports imports decls) = do
  checkModulePragmas pragmas
  modify $ \s -> s { stateModuleName = modulename
                   , stateExportAll = isNothing exports
                   }
  mapM_ emitExport (fromMaybe [] exports)
  imported <- fmap concat (mapM compileImport imports)
  current <- compileDecls True decls
  return (imported ++ current)
compileModule mod = throwError (UnsupportedModuleSyntax mod)

warn :: String -> Compile ()
warn "" = return ()
warn w = do
  shouldWarn <- configWarn <$> gets stateConfig
  when shouldWarn . liftIO . hPutStrLn stderr $ "Warning: " ++ w

checkModulePragmas :: [ModulePragma] -> Compile ()
checkModulePragmas pragmas =
  when (not $ any noImplicitPrelude pragmas) $
    warn "NoImplicitPrelude not specified"
  where
    noImplicitPrelude :: ModulePragma -> Bool
    noImplicitPrelude (LanguagePragma _ names) = any (== (Ident "NoImplicitPrelude")) names
    noImplicitPrelude _ = False

instance CompilesTo Module [JsStmt] where compileTo = compileModule

findImport :: [FilePath] -> String -> IO String
findImport (dir:dirs) name = do
  exists <- doesFileExist path
  if exists
    then readFile path
    else findImport dirs name
  where
    path = dir </> replace '.' '/' name ++ ".hs"
    replace c r = map (\x -> if x == c then r else x)
findImport [] name =
  error $ "Could not find import: " ++ name

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ (ModuleName name) False _ Nothing Nothing Nothing) = do
  unlessImported name $ do
    dirs <- configDirectoryIncludes <$> gets stateConfig
    contents <- io (findImport dirs name)
    state <- gets id
    result <- liftIO $ compileToAst state compileModule contents
    case result of
      Right (stmts,state) -> do
        modify $ \s -> s { stateFayToJs = stateFayToJs state
                         , stateJsToFay = stateJsToFay state
                         , stateImported = stateImported state
                         }
        return stmts
      Left err -> throwError err
compileImport i =
  error $ "compileImport: Import syntax not supported. " ++
        "The compiler writer was too lazy to support that.\n" ++
        "It was: " ++ show i

-- | Don't re-import the same modules.
unlessImported :: String -> Compile [JsStmt] -> Compile [JsStmt]
unlessImported name importIt = do
  imported <- gets stateImported
  if elem name imported
     then return []
     else do
       modify $ \s -> s { stateImported = name : imported }
       importIt

-- | Compile Haskell declaration.
compileDecls :: Bool -> [Decl] -> Compile [JsStmt]
compileDecls toplevel decls =
  case decls of
    [] -> return []
    (TypeSig _ _ sig:bind@PatBind{}:decls) -> appendM (compilePatBind toplevel (Just sig) bind)
                                                      (compileDecls toplevel decls)
    (decl:decls) -> appendM (compileDecl toplevel decl)
                            (compileDecls toplevel decls)

  where appendM m n = do x <- m
                         xs <- n
                         return (x ++ xs)

-- | Compile a declaration.
compileDecl :: Bool -> Decl -> Compile [JsStmt]
compileDecl toplevel decl =
  case decl of
    pat@PatBind{} -> compilePatBind toplevel Nothing pat
    FunBind matches -> compileFunCase toplevel matches
    DataDecl _ DataType _ _ _ constructors _ -> compileDataDecl toplevel decl constructors
    GDataDecl _ DataType _l _i _v _n decls _ -> compileDataDecl toplevel decl (map convertGADT decls)
    -- Just ignore type aliases and signatures.
    TypeDecl{} -> return []
    TypeSig{} -> return []
    InfixDecl{} -> return []
    ClassDecl{} -> return []
    InstDecl{} -> return [] -- FIXME: Ignore.
    DerivDecl{} -> return []
    _ -> throwError (UnsupportedDeclaration decl)

instance CompilesTo Decl [JsStmt] where compileTo = compileDecl True

-- | Compile a top-level pattern bind.
compilePatBind :: Bool -> Maybe Type -> Decl -> Compile [JsStmt]
compilePatBind toplevel sig pat =
  case pat of
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) (BDecls []) ->
      case ffiExp rhs of
        Just formatstr -> case sig of
          Just sig -> compileFFI srcloc ident formatstr sig
          Nothing  -> throwError (FfiNeedsTypeSig pat)
        _ -> compileUnguardedRhs srcloc toplevel ident rhs
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) bdecls ->
      compileUnguardedRhs srcloc toplevel ident (Let bdecls rhs)
    _ -> throwError (UnsupportedDeclaration pat)

  where ffiExp (App (Var (UnQual (Ident "ffi"))) (Lit (String formatstr))) = Just formatstr
        ffiExp _ = Nothing

-- | Compile an FFI call.
compileFFI :: SrcLoc -- ^ Location of the original FFI decl.
           -> Name   -- ^ Name of the to-be binding.
           -> String -- ^ The format string.
           -> Type   -- ^ Type signature.
           -> Compile [JsStmt]
compileFFI srcloc name formatstr sig = do
  inner <- formatFFI formatstr (zip params funcFundamentalTypes)
  case JS.parse JS.parseExpression (prettyPrint name) (printJSString (wrapReturn inner)) of
    Left err -> throwError (FfiFormatInvalidJavaScript inner (show err))
    Right{}  -> fmap return (bindToplevel srcloc True (UnQual name) (body inner))

  where body inner = foldr wrapParam (wrapReturn inner) params
        wrapParam name inner = JsFun [name] [] (Just inner)
        params = zipWith const uniqueNames [1..typeArity sig]
        wrapReturn inner = thunk $
          case lastMay funcFundamentalTypes of
            -- Returns a “pure” value;
            Just{} -> jsToFay returnType (JsRawExp inner)
            -- Base case:
            Nothing -> JsRawExp inner
        funcFundamentalTypes = functionTypeArgs sig
        returnType = last funcFundamentalTypes

-- | Format the FFI format string with the given arguments.
formatFFI :: String                      -- ^ The format string.
          -> [(JsParam,FundamentalType)] -- ^ Arguments.
          -> Compile String              -- ^ The JS code.
formatFFI formatstr args = go formatstr where
  go ('%':'*':xs) = do
    these <- mapM inject (zipWith const [1..] args)
    rest <- go xs
    return (intercalate "," these ++ rest)
  go ('%':'%':xs) = do
    rest <- go xs
    return ('%' : rest)
  go ['%'] = throwError FfiFormatIncompleteArg
  go ('%':(span isDigit -> (op,xs))) =
    case readMay op of
     Nothing -> throwError (FfiFormatBadChars op)
     Just n -> do
       this <- inject n
       rest <- go xs
       return (this ++ rest)
  go (x:xs) = do rest <- go xs
                 return (x : rest)
  go [] = return []

  inject n =
    case listToMaybe (drop (n-1) args) of
      Nothing -> throwError (FfiFormatNoSuchArg n)
      Just (arg,typ) -> do
        return (printJSString (fayToJs (typeRep typ) (JsName arg)))

-- | Translate: Fay → JS.
fayToJs :: JsExp -> JsExp -> JsExp
fayToJs typ exp = JsApp (JsName (hjIdent "fayToJs"))
                        [typ,exp]

-- | Get a JS-representation of a fundamental type for encoding/decoding.
typeRep :: FundamentalType -> JsExp
typeRep typ =
  case typ of
    FunctionType xs     -> JsList [JsLit $ JsStr "function",JsList (map typeRep xs)]
    JsType x            -> JsList [JsLit $ JsStr "action",JsList [typeRep x]]
    ListType x          -> JsList [JsLit $ JsStr "list",JsList [typeRep x]]
    UserDefined name xs -> JsList [JsLit $ JsStr "user"
                                  ,JsLit $ JsStr (unname name)
                                  ,JsList (map typeRep xs)]
    typ -> JsList [JsLit $ JsStr nom]

      where nom = case typ of
              StringType -> "string"
              DoubleType -> "double"
              IntType    -> "int"
              BoolType   -> "bool"
              DateType   -> "date"
              _          -> "unknown"

-- | Get arg types of a function type.
functionTypeArgs :: Type -> [FundamentalType]
functionTypeArgs t =
  case t of
    TyForall _ _ i -> functionTypeArgs i
    TyFun a b      -> argType a : functionTypeArgs b
    TyParen st     -> functionTypeArgs st
    r              -> [argType r]

-- | Convert a Haskell type to an internal FFI representation.
argType :: Type -> FundamentalType
argType t =
  case t of
    TyCon "String"        -> StringType
    TyCon "Double"        -> DoubleType
    TyCon "Int"           -> IntType
    TyCon "Bool"          -> BoolType
    TyApp (TyCon "Fay") a -> JsType (argType a)
    TyFun x xs            -> FunctionType (argType x : functionTypeArgs xs)
    TyList x              -> ListType (argType x)
    TyParen st            -> argType st
    TyApp op arg          -> userDefined (reverse (arg : expandApp op))
    _                     ->
      -- No semantic point to this, merely to avoid GHC's broken
      -- warning.
      case t of
        TyCon (UnQual user)   -> UserDefined user []
        _ -> UnknownType

-- | Generate a user-defined type.
userDefined :: [Type] -> FundamentalType
userDefined (TyCon (UnQual name):typs) = UserDefined name (map argType typs)
userDefined _ = UnknownType

-- | Expand a type application.
expandApp :: Type -> [Type]
expandApp (TyParen t) = expandApp t
expandApp (TyApp op arg) = arg : expandApp op
expandApp x = [x]

-- | Get the arity of a type.
typeArity :: Type -> Int
typeArity t =
  case t of
    TyForall _ _ i -> typeArity i
    TyFun _ b      -> 1 + typeArity b
    TyParen st     -> typeArity st
    _              -> 0

-- | Compile a normal simple pattern binding.
compileUnguardedRhs :: SrcLoc -> Bool -> Name -> Exp -> Compile [JsStmt]
compileUnguardedRhs srcloc toplevel ident rhs = do
  body <- compileExp rhs
  bind <- bindToplevel srcloc toplevel (UnQual ident) (thunk body)
  return [bind]

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

-- | Compile a data declaration.
compileDataDecl :: Bool -> Decl -> [QualConDecl] -> Compile [JsStmt]
compileDataDecl toplevel _decl constructors =
  fmap concat $
    forM constructors $ \(QualConDecl srcloc _ _ condecl) ->
      case condecl of
        ConDecl (UnQual -> name) types  -> do
          let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
              fields' = (zip (map return fields) types)
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          return [cons, func]
        InfixConDecl t1 (UnQual -> name) t2 -> do
          let slots = [Ident "slot1", Ident "slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name fields
          emitJsToFay name fields
          return [cons, func]
        RecDecl (UnQual -> name) fields' -> do
          let fields = concatMap fst fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors srcloc fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          return (cons : func : funs)

  where
    -- Creates a constructor R_RecConstr for a Record
    makeConstructor name fields = do
          let fieldParams = map (fromString . unname) fields
          return $
            JsVar (constructorName name) $
              JsFun fieldParams
                  (flip map fields $ \field@(Ident s) ->
                     JsSetProp (fromString ":this") (UnQual field) (JsName (fromString s)))
                Nothing

    -- Creates a function to initialize the record by regular application
    makeFunc name fields = do
          let fieldParams = map (\(Ident s) -> fromString s) fields
          let fieldExps = map (JsName . UnQual) fields
          return $ JsVar name $
            foldr (\slot inner -> JsFun [slot] [] (Just inner))
              (thunk $ JsNew (constructorName name) fieldExps)
              fieldParams

    -- Creates getters for a RecDecl's values
    makeAccessors srcloc fields =
      forM fields $ \(Ident name) ->
           bindToplevel srcloc
                        toplevel
                        (fromString name)
                        (JsFun ["x"]
                               []
                               (Just (thunk (JsGetProp (force (JsName "x"))
                                                       (fromString name)))))

fayToJsDispatcher :: [JsStmt] -> JsStmt
fayToJsDispatcher cases =
  JsVar (hjIdent "fayToJsUserDefined")
        (JsFun ["type",transcodingObj]
               (decl ++ cases ++ [baseCase])
               Nothing)

  where decl = [JsVar transcodingObjForced
                      (force (JsName transcodingObj))
               ,JsVar "argTypes"
                      (JsLookup (JsName "type")
                                (JsLit (JsInt 2)))]
        baseCase =
          JsEarlyReturn (JsName transcodingObj)
          -- JsThrow (JsNew "Error"
          --                [JsLit (JsStr "No handler for translating this Fay value to a JS value.")])

-- Make a Fay→JS encoder.
emitFayToJs :: QName -> [([Name], BangType)] -> Compile ()
emitFayToJs name (explodeFields -> fieldTypes) =
  modify $ \s -> s { stateFayToJs = translator : stateFayToJs s }

  where
    translator = JsIf (JsInstanceOf (JsName transcodingObjForced) (constructorName name))
                      [JsEarlyReturn (JsObj (("instance",JsLit (JsStr (qname name)))
                                                     : zipWith declField [0..] fieldTypes))]
                      []
    -- Declare/encode Fay→JS field
    declField :: Int -> (Name,BangType) -> (String,JsExp)
    declField _i (name,typ) =
      (unname name
      ,fayToJs (case argType (bangType typ) of
                 known -> typeRep known)
               (force (JsGetProp (JsName transcodingObjForced)
                                 (UnQual name))))

jsToFayDispatcher :: [JsStmt] -> JsStmt
jsToFayDispatcher cases =
  JsVar (hjIdent "jsToFayUserDefined")
        (JsFun ["type",transcodingObj]
               (cases ++ [baseCase])
               Nothing)

  where baseCase =
          JsEarlyReturn (JsName transcodingObj)
          -- JsThrow (JsNew "Error"
          --                [JsLit (JsStr "No handler for translating this JS value to a Fay value.")])

-- Make a JS→Fay decoder
emitJsToFay ::  QName -> [([Name], BangType)] -> Compile ()
emitJsToFay name (explodeFields -> fieldTypes) =
  modify $ \s -> s { stateJsToFay = translator : stateJsToFay s }

  where
    translator =
      JsIf (JsEq (JsGetPropExtern (JsName transcodingObj) "instance")
                 (JsLit (JsStr (qname name))))
           [JsEarlyReturn (JsNew (constructorName name)
                                 (map decodeField fieldTypes))]
           []
    -- Decode JS→Fay field
    decodeField :: (Name,BangType) -> JsExp
    decodeField (name,typ) =
      jsToFay (argType (bangType typ))
              (JsGetPropExtern (JsName transcodingObj)
                               (unname name))

explodeFields :: [([a], t)] -> [(a, t)]
explodeFields = concatMap $ \(names,typ) -> map (,typ) names

transcodingObj :: JsName
transcodingObj = "obj"

transcodingObjForced :: JsName
transcodingObjForced = "_obj"

-- | Extract the type.
bangType :: BangType -> Type
bangType typ =
  case typ of
    BangedTy ty   -> ty
    UnBangedTy ty -> ty
    UnpackedTy ty -> ty

-- | Extract the string from a qname.
qname :: QName -> String
qname (UnQual (Ident str)) = str
qname (UnQual (Symbol sym)) = jsEncodeName sym
qname i = error $ "qname: Expected unqualified ident, found: " ++ show i -- FIXME:

-- | Extra the string from an ident.
unname :: Name -> String
unname (Ident str) = str
unname _ = error "Expected ident from uname." -- FIXME:

constructorName :: QName -> QName
constructorName = fromString . ("$_" ++) . qname

-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel matches@(Match srcloc name argslen _ _ _:_) = do
  tco  <- config configTCO
  pats <- fmap optimizePatConditions (mapM compileCase matches)
  bind <- bindToplevel srcloc
                       toplevel
                       (UnQual name)
                       (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                              (stmtsThunk (let stmts = (concat pats ++ basecase)
                                           in if tco
                                                 then optimizeTailCalls args name stmts
                                                 else stmts))
                              args)
  return [bind]
  where args = zipWith const uniqueNames argslen

        isWildCardMatch (Match _ _ pats _ _ _) = all isWildCardPat pats

        compileCase :: Match -> Compile [JsStmt]
        compileCase match@(Match _ _ pats _ rhs _) = do
          whereDecls' <- whereDecls match
          exp  <- compileRhs rhs
          body <- if null whereDecls'
                    then return exp
                    else do
                        binds <- mapM compileLetDecl whereDecls'
                        return (JsApp (JsFun [] (concat binds) (Just exp)) [])
          foldM (\inner (arg,pat) ->
                  compilePat (JsName arg) pat inner)
                [JsEarlyReturn body]
                (zip args pats)

        whereDecls :: Match -> Compile [Decl]
        whereDecls (Match _ _ _ _ _ (BDecls decls)) = return decls
        whereDecls match = throwError (UnsupportedWhereInMatch match)

        basecase :: [JsStmt]
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ show name)
                                  (JsList (map JsName args))]

-- | Optimize functions in tail-call form.
optimizeTailCalls :: [JsParam] -- ^ The function parameters.
                  -> Name      -- ^ The function name.
                  -> [JsStmt]  -- ^ The body of the function.
                  -> [JsStmt]  -- ^ A new optimized function body.
optimizeTailCalls params name stmts = abandonIfNoChange $
  JsWhile (JsLit (JsBool True))
          (concatMap replaceTailStmt
                     (reverse (zip (reverse stmts) [0::Integer ..])))

  where replaceTailStmt (JsIf cond sothen orelse,i) = [JsIf cond (concatMap (replaceTailStmt . (,i)) sothen)
                                                                 (concatMap (replaceTailStmt . (,i)) orelse)]
        replaceTailStmt (JsEarlyReturn exp,i) = expTailReplace i exp
        replaceTailStmt (x,_) = [x]
        expTailReplace i (flatten -> Just (JsName (UnQual call):args@(_:_)))
          | call == name = updateParamsInstead i args
        expTailReplace _i original = [JsEarlyReturn original]
        updateParamsInstead i args = zipWith JsUpdate params args ++
                                     [JsContinue | i /= 0]
        abandonIfNoChange (JsWhile _ newstmts)
          | newstmts == stmts = stmts
        abandonIfNoChange new = [new]

-- | Flatten an application expression into function : arg : arg : []
flatten :: JsExp -> Maybe [JsExp]
flatten (JsApp op@JsApp{} arg) = do
  inner <- expand op
  return (inner ++ arg)
flatten name@JsName{} = return [name]
flatten _ = Nothing

-- | Expand a forced value into the value.
expand :: JsExp -> Maybe [JsExp]
expand (JsApp (JsName (UnQual (Ident "_"))) xs) =
  fmap concat (mapM flatten xs)
expand _ = Nothing

-- | Format a JS string using "js-beautify", or return the JS as-is if
--   "js-beautify" is unavailable.
prettyPrintString :: String -> IO String
prettyPrintString contents = do
    mexe <- findExecutable "js-beautify"
    case mexe of
      Nothing -> return $ contents ++ "\n"
      Just exe -> do
        (code,out,_) <- readProcessWithExitCode exe ["--stdin"] contents
        case code of
          ExitSuccess -> return out
          ExitFailure _ -> return $ contents ++ "\n"

-- | Compile a right-hand-side expression.
compileRhs :: Rhs -> Compile JsExp
compileRhs (UnGuardedRhs exp) = compileExp exp
compileRhs (GuardedRhss rhss) = compileGuards rhss

-- | Compile guards
compileGuards :: [GuardedRhs] -> Compile JsExp
compileGuards [] = return . JsThrowExp . JsLit . JsStr $ "Non-exhaustive guards"
compileGuards ((GuardedRhs _ (Qualifier (Var (UnQual (Ident "otherwise"))):_) exp):_) = compileExp exp
compileGuards (GuardedRhs _ (Qualifier guard:_) exp : rest) =
  JsTernaryIf <$> fmap force (compileExp guard)
              <*> compileExp exp
              <*> compileGuards rest
compileGuards rhss = throwError . UnsupportedRhs . GuardedRhss $ rhss

-- | Compile Haskell expression.
compileExp :: Exp -> Compile JsExp
compileExp exp =
  case exp of
    Paren exp                     -> compileExp exp
-- Commented out: See #59
--    Var (UnQual (Ident "return")) -> return (JsName (hjIdent "return"))
    Var qname                     -> return (JsName qname)
    Lit lit                       -> compileLit lit
    App exp1 exp2                 -> compileApp exp1 exp2
    NegApp exp                    -> compileNegApp exp
    InfixApp exp1 op exp2         -> compileInfixApp exp1 op exp2
    Let (BDecls decls) exp        -> compileLet decls exp
    List []                       -> return JsNull
    List xs                       -> compileList xs
    Tuple xs                      -> compileList xs
    If cond conseq alt            -> compileIf cond conseq alt
    Case exp alts                 -> compileCase exp alts
    Con (UnQual (Ident "True"))   -> return (JsLit (JsBool True))
    Con (UnQual (Ident "False"))  -> return (JsLit (JsBool False))
    Con exp                       -> return (JsName exp)
    Do stmts                      -> compileDoBlock stmts
    Lambda _ pats exp             -> compileLambda pats exp
    EnumFrom i                    -> do e <- compileExp i
                                        return (JsApp (JsName "enumFrom") [e])
    EnumFromTo i i'               -> do f <- compileExp i
                                        t <- compileExp i'
                                        return (JsApp (JsApp (JsName "enumFromTo") [f])
                                                      [t])
    RecConstr name fieldUpdates -> compileRecConstr name fieldUpdates
    RecUpdate rec  fieldUpdates -> updateRec rec fieldUpdates
    ExpTypeSig _ e _ -> compileExp e

    exp -> throwError (UnsupportedExpression exp)

instance CompilesTo Exp JsExp where compileTo = compileExp


-- | Compile simple application.
compileApp :: Exp -> Exp -> Compile JsExp
compileApp exp1 exp2 = do
   flattenApps <- config configFlattenApps
   if flattenApps then method2 else method1
   where
  -- Method 1:
  -- In this approach code ends up looking like this:
  -- a(a(a(a(a(a(a(a(a(a(L)(c))(b))(0))(0))(y))(t))(a(a(F)(3*a(a(d)+a(a(f)/20))))*a(a(f)/2)))(140+a(f)))(y))(t)})
  -- Which might be OK for speed, but increases the JS stack a fair bit.
  method1 =
    JsApp <$> (forceFlatName <$> compileExp exp1)
          <*> fmap return (compileExp exp2)
  forceFlatName name = JsApp (JsName "_") [name]

  -- Method 2:
  -- In this approach code ends up looking like this:
  -- d(O,a,b,0,0,B,w,e(d(I,3*e(e(c)+e(e(g)/20))))*e(e(g)/2),140+e(g),B,w)}),d(K,g,e(c)+0.05))
  -- Which should be much better for the stack and readability, but probably not great for speed.
  method2 = fmap flatten $
    JsApp <$> compileExp exp1
          <*> fmap return (compileExp exp2)
  flatten (JsApp op args) =
   case op of
     JsApp l r -> JsApp l (r ++ args)
     _        -> JsApp (JsName "__") (op : args)
  flatten x = x

-- | Compile a negate application
compileNegApp :: Exp -> Compile JsExp
compileNegApp e = JsNegApp . force <$> compileExp e

-- | Compile an infix application, optimizing the JS cases.
compileInfixApp :: Exp -> QOp -> Exp -> Compile JsExp
compileInfixApp exp1 op exp2 = do
  case getOp op of
    UnQual (Symbol symbol)
      | symbol `elem` words "* + - / < > || &&" -> do
          e1 <- compileExp exp1
          e2 <- compileExp exp2
          fn <- resolveOpToVar >=> compileExp $ op
          return $ JsApp (JsApp (force fn) [(force e1)]) [(force e2)]
    _ -> do
      var <- resolveOpToVar op
      compileExp (App (App var exp1) exp2)

  where getOp (QVarOp op) = op
        getOp (QConOp op) = op

-- | Compile a list expression.
compileList :: [Exp] -> Compile JsExp
compileList xs = do
  exps <- mapM compileExp xs
  return (makeList exps)

makeList :: [JsExp] -> JsExp
makeList exps = (JsApp (JsName (hjIdent "list")) [JsList exps])

-- | Compile an if.
compileIf :: Exp -> Exp -> Exp -> Compile JsExp
compileIf cond conseq alt =
  JsTernaryIf <$> fmap force (compileExp cond)
              <*> compileExp conseq
              <*> compileExp alt

-- | Compile a lambda.
compileLambda :: [Pat] -> Exp -> Compile JsExp
compileLambda pats exp = do
  exp <- compileExp exp
  stmts <- foldM (\inner (param,pat) -> do
                   stmts <- compilePat (JsName param) pat inner
                   return [JsEarlyReturn (JsFun [param] (stmts ++ [unhandledcase param | not allfree]) Nothing)])
                 [JsEarlyReturn exp]
                 (reverse (zip uniqueNames pats))
  case stmts of
    [JsEarlyReturn fun@JsFun{}] -> return fun
    _ -> error "Unexpected statements in compileLambda"

  where unhandledcase = throw "unhandled case" . JsName
        allfree = all isWildCardPat pats

-- | Compile case expressions.
compileCase :: Exp -> [Alt] -> Compile JsExp
compileCase exp alts = do
  exp <- compileExp exp
  withScopedTmpName $ \tmpName -> do
    pats <- fmap optimizePatConditions $ mapM (compilePatAlt (JsName tmpName)) alts
    return $
      JsApp (JsFun [tmpName]
                   (concat pats)
                   (if any isWildCardAlt alts
                       then Nothing
                       else Just (throwExp "unhandled case" (JsName tmpName))))
            [exp]

-- | Compile a do block.
compileDoBlock :: [Stmt] -> Compile JsExp
compileDoBlock stmts = do
  doblock <- foldM compileStmt Nothing (reverse stmts)
  maybe (throwError EmptyDoBlock) compileExp doblock

-- | Compile a statement of a do block.
compileStmt :: Maybe Exp -> Stmt -> Compile (Maybe Exp)
compileStmt inner stmt =
  case inner of
    Nothing -> initStmt
    Just inner -> subsequentStmt inner

  where initStmt =
          case stmt of
            Qualifier exp -> return (Just exp)
            LetStmt{}     -> throwError LetUnsupported
            _             -> throwError InvalidDoBlock

        subsequentStmt inner =
          case stmt of
            Generator loc pat exp -> compileGenerator loc pat inner exp
            Qualifier exp -> return (Just (InfixApp exp
                                                    (QVarOp (UnQual (Symbol ">>")))
                                                    inner))
            LetStmt (BDecls binds) -> return (Just (Let (BDecls binds) inner))
            LetStmt _ -> throwError LetUnsupported
            RecStmt{} -> throwError RecursiveDoUnsupported

        compileGenerator srcloc pat inner exp = do
          let body = Lambda srcloc [pat] inner
          return (Just (InfixApp exp
                                 (QVarOp (UnQual (Symbol ">>=")))
                                 body))

-- | Compile the given pattern against the given expression.
compilePatAlt :: JsExp -> Alt -> Compile [JsStmt]
compilePatAlt exp (Alt _ pat rhs _) = do
  alt <- compileGuardedAlt rhs
  compilePat exp pat [JsEarlyReturn alt]

-- | Compile the given pattern against the given expression.
compilePat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePat exp pat body =
  case pat of
    PVar name       -> return $ JsVar (UnQual name) exp : body
    PApp cons pats  -> compilePApp cons pats exp body
    PLit literal    -> compilePLit exp literal body
    PParen pat      -> compilePat exp pat body
    PWildCard       -> return body
    pat@PInfixApp{} -> compileInfixPat exp pat body
    PList pats      -> compilePList pats body exp
    PTuple pats     -> compilePList pats body exp
    PAsPat name pat -> compilePAsPat exp name pat body
    PRec name pats  -> compilePatFields exp name pats body
    pat             -> throwError (UnsupportedPattern pat)

-- | Compile a record field pattern.
compilePatFields :: JsExp -> QName -> [PatField] -> [JsStmt] -> Compile [JsStmt]
compilePatFields exp (Ident . qname -> name) pats body = do
    c <- liftM (++ body) (compilePats' [] pats)
    return [JsIf ((force exp) `JsInstanceOf` constructorName (UnQual name)) c []]
  where -- compilePats' collects field names that had already been matched so that
        -- wildcard generates code for the rest of the fields.
        compilePats' :: [Name] -> [PatField] -> Compile [JsStmt]
        compilePats' names (PFieldPun name:xs) =
          compilePats' names (PFieldPat (UnQual name) (PVar name):xs)

        compilePats' names (PFieldPat fieldQName (PVar varName):xs) = do
          let fieldname = case fieldQName of
                            (Qual _ name) -> name
                            (UnQual name) -> name
                            (Special _) -> undefined -- TODO: how to handle special names ?
          r <- compilePats' (fieldname:names) xs
          return $ JsVar (UnQual varName) (JsGetProp (force exp) fieldQName):r -- TODO: think about this force call

        compilePats' names (PFieldWildcard:xs) = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
              fields' = fields \\ names
              f = map (\fieldName -> JsVar (UnQual fieldName) (JsGetProp (force exp) (UnQual fieldName))) fields'
          r <- compilePats' names xs
          return $ f ++ r

        compilePats' _ [] = return []

        compilePats' _ (pat:_) = throwError (UnsupportedFieldPattern pat)

-- | Compile a literal value from a pattern match.
compilePLit :: JsExp -> Literal -> [JsStmt] -> Compile [JsStmt]
compilePLit exp literal body = do
  lit <- compileLit literal
  return [JsIf (equalExps exp lit)
               body
               []]

-- | Compile as binding in pattern match
compilePAsPat :: JsExp -> Name -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePAsPat exp name pat body = do
  x <- compilePat exp pat body
  return ([JsVar (UnQual name) exp] ++ x ++ body)

-- | Compile a record construction with named fields
-- | GHC will warn on uninitialized fields, they will be undefined in JS.
compileRecConstr :: QName -> [FieldUpdate] -> Compile JsExp
compileRecConstr name fieldUpdates = do
    let o = UnQual (Ident (qname name))
    -- var obj = new $_Type()
    let record = JsVar o (JsNew (constructorName name) [])
    setFields <- liftM concat (forM fieldUpdates (updateStmt o))
    return $ JsApp (JsFun [] (record:setFields) (Just (JsName o))) []
  where updateStmt :: QName -> FieldUpdate -> Compile [JsStmt]
        updateStmt o (FieldUpdate field value) = do
          exp <- compileExp value
          return [JsSetProp o field exp]
        updateStmt o@(UnQual name) FieldWildcard = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
          return (map (\fieldName -> JsSetProp o (UnQual fieldName) (JsName (UnQual fieldName))) fields)
        -- TODO: FieldPun
        -- I couldn't find a code that generates (FieldUpdate (FieldPun ..))
        updateStmt _ u = error ("updateStmt: " ++ show u)

updateRec :: Exp -> [FieldUpdate] -> Compile JsExp
updateRec rec fieldUpdates = do
    record <- force <$> compileExp rec
    let copyName = UnQual (Ident "$_record_to_update")
        copy = JsVar copyName
                     (JsRawExp ("Object.create(" ++ printJSString record ++ ")"))
    setFields <- forM fieldUpdates (updateExp copyName)
    return $ JsApp (JsFun [] (copy:setFields) (Just (JsName copyName))) []
  where updateExp :: QName -> FieldUpdate -> Compile JsStmt
        updateExp copyName (FieldUpdate field value) =
          JsSetProp copyName field <$> compileExp value
        updateExp copyName (FieldPun name) =
          -- let a = 1 in C {a}
          return $ JsSetProp copyName (UnQual name) (JsName (UnQual name))
        -- TODO: FieldWildcard
        -- I also couldn't find a code that generates (FieldUpdate FieldWildCard)
        updateExp _ FieldWildcard = error "unsupported update: FieldWildcard"

-- | Equality test for two expressions, with some optimizations.
equalExps :: JsExp -> JsExp -> JsExp
equalExps a b
  | isConstant a && isConstant b = JsEq a b
  | isConstant a = JsEq a (force b)
  | isConstant b = JsEq (force a) b
  | otherwise =
     JsApp (JsName (hjIdent "equal")) [a,b]

-- | Is a JS expression a literal (constant)?
isConstant :: JsExp -> Bool
isConstant JsLit{} = True
isConstant _       = False

-- | Compile a pattern application.
compilePApp :: QName -> [Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePApp cons pats exp body = do
  let forcedExp = force exp
  let boolIf b = return [JsIf (JsEq forcedExp (JsLit (JsBool b))) body []]
  case cons of
    -- Special-casing on the booleans.
    "True" -> boolIf True
    "False" -> boolIf False
    -- Everything else, generic:
    _ -> do
      rf <- lookup (Ident (qname cons)) <$> gets stateRecords
      let recordFields =
            fromMaybe
              (error $ "Constructor '" ++ qname cons ++
                       "' was not found in stateRecords, did you try running this through GHC first?")
              rf
      substmts <- foldM (\body (Ident field,pat) ->
                             compilePat (JsGetProp forcedExp (fromString field)) pat body)
                  body
                  (reverse (zip recordFields pats))
      return [JsIf (forcedExp `JsInstanceOf` constructorName cons)
                   substmts
                   []]

-- | Compile a pattern list.
compilePList :: [Pat] -> [JsStmt] -> JsExp -> Compile [JsStmt]
compilePList [] body exp =
  return [JsIf (JsEq (force exp) JsNull) body []]
compilePList pats body exp = do
  let forcedExp = force exp
  foldM (\body (i,pat) -> compilePat (JsApp (JsApp (JsName (hjIdent "index"))
                                                   [JsLit (JsInt i)])
                                            [forcedExp])
                                     pat body)
        body
        (reverse (zip [0..] pats))

-- | Compile an infix pattern (e.g. cons and tuples.)
compileInfixPat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp left (Special cons) right) body =
  case cons of
    Cons -> do
      withScopedTmpName $ \tmpName -> do
        let forcedExp = JsName tmpName
            x = JsGetProp forcedExp "car"
            xs = JsGetProp forcedExp "cdr"
        rightMatch <- compilePat xs right body
        leftMatch <- compilePat x left rightMatch
        return [JsVar tmpName (force exp)
               ,JsIf (JsInstanceOf forcedExp (hjIdent "Cons"))
                     leftMatch
                     []]
    _ -> throwError (UnsupportedPattern pat)
compileInfixPat _ pat _ = throwError (UnsupportedPattern pat)

-- | Compile a guarded alt.
compileGuardedAlt :: GuardedAlts -> Compile JsExp
compileGuardedAlt alt =
  case alt of
    UnGuardedAlt exp -> compileExp exp
    alt -> throwError (UnsupportedGuardedAlts alt)

-- | Compile a let expression.
compileLet :: [Decl] -> Exp -> Compile JsExp
compileLet decls exp = do
  body <- compileExp exp
  binds <- mapM compileLetDecl decls
  return (JsApp (JsFun [] (concat binds) (Just body)) [])

-- | Compile let declaration.
compileLetDecl :: Decl -> Compile [JsStmt]
compileLetDecl decl =
  case decl of
    decl@PatBind{} -> compileDecls False [decl]
    decl@FunBind{} -> compileDecls False [decl]
    TypeSig{}      -> return []
    _              -> throwError (UnsupportedLetBinding decl)

-- | Compile Haskell literal.
compileLit :: Literal -> Compile JsExp
compileLit lit =
  case lit of
    Char ch       -> return (JsLit (JsChar ch))
    Int integer   -> return (JsLit (JsInt (fromIntegral integer))) -- FIXME:
    Frac rational -> return (JsLit (JsFloating (fromRational rational)))
    -- TODO: Use real JS strings instead of array, probably it will
    -- lead to the same result.
    String string -> return (JsApp (JsName (hjIdent "list"))
                                   [JsLit (JsStr string)])
    lit           -> throwError (UnsupportedLiteral lit)

--------------------------------------------------------------------------------
-- Compilation utilities

-- | Generate unique names.
uniqueNames :: [JsParam]
uniqueNames = map (fromString . ("$_" ++))
            $ map return "abcxyz" ++
              zipWith (:) (cycle "v")
                          (map show [1 :: Integer ..])

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
throw msg exp = JsThrow (JsList [JsLit (JsStr msg),exp])

-- | Throw a JS exception (in an expression).
throwExp :: String -> JsExp -> JsExp
throwExp msg exp = JsThrowExp (JsList [JsLit (JsStr msg),exp])

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

-- | Wrap an expression in a thunk.
thunk :: JsExp -> JsExp
-- thunk exp = JsNew (hjIdent "Thunk") [JsFun [] [] (Just exp)]
thunk exp =
  case exp of
    -- JS constants don't need to be in thunks, they're already strict.
    JsLit{} -> exp
    JsName "true" -> exp
    JsName "false" -> exp
    -- Functions (e.g. lets) used for introducing a new lexical scope
    -- aren't necessary inside a thunk. This is a simple aesthetic
    -- optimization.
    JsApp fun@JsFun{} [] -> JsNew ":thunk" [fun]
    -- Otherwise make a regular thunk.
    _ -> JsNew ":thunk" [JsFun [] [] (Just exp)]

-- | Wrap an expression in a thunk.
stmtsThunk :: [JsStmt] -> JsExp
stmtsThunk stmts = JsNew ":thunk" [JsFun [] stmts Nothing]

-- | Translate: JS → Fay.
jsToFay :: FundamentalType -> JsExp -> JsExp
jsToFay typ exp = JsApp (JsName (hjIdent "jsToFay"))
                        [typeRep typ,exp]

-- | Force an expression in a thunk.
force :: JsExp -> JsExp
force exp
  | isConstant exp = exp
  | otherwise = JsApp (JsName "_") [exp]

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

  where getOp (QVarOp op) = op
        getOp (QConOp op) = op

-- | Make an identifier from the built-in HJ module.
hjIdent :: String -> QName
hjIdent = Qual (ModuleName "Fay") . Ident

-- | Make a top-level binding.
bindToplevel :: SrcLoc -> Bool -> QName -> JsExp -> Compile JsStmt
bindToplevel srcloc toplevel name exp = do
  exportAll <- gets stateExportAll
  when (toplevel && exportAll) $ emitExport (EVar name)
  return (JsMappedVar srcloc name exp)

-- | Emit exported names.
emitExport :: ExportSpec -> Compile ()
emitExport spec =
  case spec of
    EVar (UnQual name) -> modify $ \s -> s { stateExports = name : stateExports s }
    EVar _             -> error "Emitted a qualifed export, not supported."
    _ -> throwError (UnsupportedExportSpec spec)

--------------------------------------------------------------------------------
-- Utilities

-- | Parse result.
parseResult :: ((SrcLoc,String) -> b) -> (a -> b) -> ParseResult a -> b
parseResult fail ok result =
  case result of
    ParseOk a -> ok a
    ParseFailed srcloc msg -> fail (srcloc,msg)

-- | Get a config option.
config :: (CompileConfig -> a) -> Compile a
config f = gets (f . stateConfig)

instance IsString Name where fromString = Ident
