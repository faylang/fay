{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -Wall #-}

-- | Compile FFI definitions.

module Fay.Compiler.FFI
  (emitFayToJs
  ,emitJsToFay
  ,compileFFI
  ,compileFFIExp
  ,jsToFayHash
  ,fayToJsHash
  ,typeArity
  ) where

import           Fay.Compiler.Misc
import           Fay.Compiler.Print                     (printJSString)
import           Fay.Compiler.QName
import           Fay.Exts.NoAnnotation                  (unAnn)
import qualified Fay.Exts.NoAnnotation                  as N
import qualified Fay.Exts.Scoped                        as S
import           Fay.Types

import           Control.Applicative                    ((<$>), (<*>))
import           Control.Arrow                          ((***))
import           Control.Monad.Error
import           Control.Monad.Writer
import           Data.Char
import           Data.Generics.Schemes
import           Data.List
import           Data.Maybe
import           Data.String
import           Language.ECMAScript3.Parser            as JS
import           Language.ECMAScript3.Syntax
import           Language.Haskell.Exts.Annotated        (SrcSpanInfo,
                                                         prettyPrint,srcInfoSpan)
import           Language.Haskell.Exts.Annotated.Syntax
import           Prelude                                hiding (exp, mod)
import           Safe

-- | Compile an FFI call.
compileFFI :: S.Name  -- ^ Name of the to-be binding.
           -> String -- ^ The format string.
           -> S.Type   -- ^ Type signature.
           -> Compile [JsStmt]
compileFFI name' formatstr sig =
  -- substitute newtypes with their child types before calling
  -- real compileFFI
  compileFFI' =<< rmNewtys sig

  where
    rmNewtys :: S.Type -> Compile N.Type
    rmNewtys (TyForall _ b c t) = TyForall () (fmap (map unAnn) b) (fmap unAnn c) <$> rmNewtys t
    rmNewtys (TyFun _ t1 t2)    = TyFun () <$> rmNewtys t1 <*> rmNewtys t2
    rmNewtys (TyTuple _ b tl)   = TyTuple () b <$> mapM rmNewtys tl
    rmNewtys (TyList _ t)       = TyList () <$> rmNewtys t
    rmNewtys (TyApp _ t1 t2)    = TyApp () <$> rmNewtys t1 <*> rmNewtys t2
    rmNewtys t@TyVar{}          = return (unAnn t)
    rmNewtys (TyCon _ qname)    = do
      newty <- lookupNewtypeConst qname
      return $ case newty of
                 Nothing     -> TyCon () (unAnn qname)
                 Just (_,ty) -> ty
    rmNewtys (TyParen _ t)      = TyParen () <$> rmNewtys t
    rmNewtys (TyInfix _ t1 q t2)= flip (TyInfix ()) (unAnn q) <$> rmNewtys t1 <*> rmNewtys t2
    rmNewtys (TyKind _ t k)     = flip (TyKind ()) (unAnn k) <$> rmNewtys t

    compileFFI' :: N.Type -> Compile [JsStmt]
    compileFFI' sig' = do
      fun <- compileFFIExp loc (Just name) formatstr sig'
      stmt <- bindToplevel True (Just (srcInfoSpan loc)) name fun
      return [stmt]

    name = unAnn name'
    loc = S.srcSpanInfo $ ann name'

-- | Compile an FFI expression (also used when compiling top level definitions).
compileFFIExp :: SrcSpanInfo -> Maybe (Name a) -> String -> (Type a) -> Compile JsExp
compileFFIExp loc (fmap unAnn -> nameopt) formatstr (unAnn -> sig) = do
  let name = fromMaybe "<exp>" nameopt
  inner <- formatFFI loc formatstr (zip params funcFundamentalTypes)
  case JS.parse JS.expression (prettyPrint name) (printJSString (wrapReturn inner)) of
    Left err -> throwError (FfiFormatInvalidJavaScript loc inner (show err))
    Right exp  -> do
      config' <- config id
      when (configGClosure config') $ warnDotUses loc inner exp
      return (body inner)

  where
    body inner = foldr wrapParam (wrapReturn inner) params
    wrapParam pname inner = JsFun Nothing [pname] [] (Just inner)
    params = zipWith const uniqueNames [1..typeArity sig]
    wrapReturn :: String -> JsExp
    wrapReturn inner = thunk $
      case lastMay funcFundamentalTypes of
        -- Returns a “pure” value;
        Just{} -> jsToFay SerializeAnywhere returnType (JsRawExp inner)
        -- Base case:
        Nothing -> JsRawExp inner
    funcFundamentalTypes = functionTypeArgs sig
    returnType = last funcFundamentalTypes

-- | Warn about uses of naked x.y which will not play nicely with Google Closure.
warnDotUses :: SrcSpanInfo -> String -> Expression SourcePos -> Compile ()
warnDotUses srcSpanInfo string expr =
  when anyrefs $
    warn $ printSrcSpanInfo srcSpanInfo ++ ":\nDot ref syntax used in FFI JS code: " ++ string

  where
    anyrefs = not (null (listify dotref expr)) ||
                  not (null (listify ldot expr))

    dotref :: Expression SourcePos -> Bool
    dotref x = case x of
      DotRef _ (VarRef _ (Id _ name)) _
         | name `elem` globalNames -> False
      DotRef{}                     -> True
      _                            -> False

    ldot :: LValue SourcePos -> Bool
    ldot x =
      case x of
        LDot _ (VarRef _ (Id _ name)) _
         | name `elem` globalNames -> False
        LDot{}                     -> True
        _                          -> False

    globalNames = ["Math","console","JSON"]

-- | Make a Fay→JS encoder.
emitFayToJs :: Name a -> [TyVarBind b] -> [([Name c], BangType d)] -> Compile ()
emitFayToJs (unAnn -> name) (map unAnn -> tyvars) (explodeFields -> fieldTypes) = do
  qname <- qualify name
  let ctrName = printJSString $ unQual qname
  tell $ mempty { writerFayToJs = [(ctrName, translator)] }

  where
    translator =
      JsFun Nothing
            [JsNameVar "type", argTypes, transcodingObjForced]
            (obj : fieldStmts (map (getIndex name tyvars) fieldTypes))
            (Just $ JsName obj_)

    obj :: JsStmt
    obj = JsVar obj_ $
      JsObj [("instance",JsLit (JsStr (printJSString (unAnn name))))]

    fieldStmts :: [(Int,(N.Name,N.BangType))] -> [JsStmt]
    fieldStmts [] = []
    fieldStmts ((i,fieldType):fts) =
      JsVar obj_v field :
        JsIf (JsNeq JsUndefined (JsName obj_v))
          [JsSetPropExtern obj_ decl (JsName obj_v)]
          [] :
        fieldStmts fts
      where
        obj_v = JsNameVar $ UnQual () (Ident () $ "obj_" ++ d)
        decl = JsNameVar $ UnQual () (Ident () d)
        (d, field) = declField i fieldType

    obj_ = JsNameVar "obj_"

    -- Declare/encode Fay→JS field
    declField :: Int -> (N.Name,N.BangType) -> (String,JsExp)
    declField i (fname,typ) =
      (prettyPrint fname
      ,fayToJs (SerializeUserArg i)
               (argType (bangType typ))
               (JsGetProp (JsName transcodingObjForced)
                          (JsNameVar (UnQual () fname))))

-- | A name used for transcoding.
transcodingObj :: JsName
transcodingObj = JsNameVar "obj"

-- | The name used for the forced version of a transcoding variable.
transcodingObjForced :: JsName
transcodingObjForced = JsNameVar "_obj"

-- | Get arg types of a function type.
functionTypeArgs :: N.Type -> [FundamentalType]
functionTypeArgs t = case t of
  TyForall _ _ _ i -> functionTypeArgs i
  TyFun _ a b      -> argType a : functionTypeArgs b
  TyParen _ st     -> functionTypeArgs st
  r                -> [argType r]

-- | Convert a Haskell type to an internal FFI representation.
argType :: N.Type -> FundamentalType
argType t = case t of
  TyCon _ (UnQual _ (Ident _ "String"))                -> StringType
  TyCon _ (UnQual _ (Ident _ "Double"))                -> DoubleType
  TyCon _ (UnQual _ (Ident _ "Int"))                   -> IntType
  TyCon _ (UnQual _ (Ident _ "Bool"))                  -> BoolType
  TyApp _ (TyCon _ (UnQual _ (Ident _ "Ptr"))) _       -> PtrType
  TyApp _ (TyCon _ (UnQual _ (Ident _ "Automatic"))) _ -> Automatic
  TyApp _ (TyCon _ (UnQual _ (Ident _ "Defined"))) a   -> Defined (argType a)
  TyApp _ (TyCon _ (UnQual _ (Ident _ "Nullable"))) a  -> Nullable (argType a)
  TyApp _ (TyCon _ (UnQual _ (Ident _ "Fay"))) a       -> JsType (argType a)
  TyFun _ x xs                  -> FunctionType (argType x : functionTypeArgs xs)
  TyList _ x                    -> ListType (argType x)
  TyTuple _ _ xs                -> TupleType (map argType xs)
  TyParen _ st                  -> argType st
  TyApp _ op arg                -> userDefined (reverse (arg : expandApp op))
  _                     ->
    -- No semantic point to this, merely to avoid GHC's broken
    -- warning.
    case t of
      TyCon _ (UnQual _ user)   -> UserDefined user []
      _ -> UnknownType

-- | Extract the type.
bangType :: N.BangType -> N.Type
bangType typ = case typ of
  BangedTy _ ty   -> ty
  UnBangedTy _ ty -> ty
  UnpackedTy _ ty -> ty

-- | Expand a type application.
expandApp :: N.Type -> [N.Type]
expandApp (TyParen _ t) = expandApp t
expandApp (TyApp _ op arg) = arg : expandApp op
expandApp x = [x]

-- | Generate a user-defined type.
userDefined :: [N.Type] -> FundamentalType
userDefined (TyCon _ (UnQual _ name):typs) = UserDefined name (map argType typs)
userDefined _ = UnknownType

-- | Translate: JS → Fay.
jsToFay :: SerializeContext -> FundamentalType -> JsExp -> JsExp
jsToFay = translate "jsToFay"
-- | Translate: Fay → JS.
fayToJs :: SerializeContext -> FundamentalType -> JsExp -> JsExp
fayToJs = translate "fayToJs"

-- | Make a translator.
translate :: String -> SerializeContext -> FundamentalType -> JsExp -> JsExp
translate method context typ exp = case typ of
  -- Unserialized types
  PtrType     -> exp
  -- Flat types
  StringType -> flat "string"
  DoubleType -> flat "double"
  IntType    -> flat "int"
  BoolType   -> flat "bool"
  -- Collapse monad
  JsType x | method == "jsToFay" -> js x
  -- Otherwise recursive stuff needs the big guns
  _ -> recursive

  where flat specialize =
          JsApp (JsName (JsBuiltIn (Ident () (method ++ "_" ++ specialize))))
                [exp]
        recursive =
          JsApp (JsName (JsBuiltIn (Ident () method)))
                [typeRep context typ
                ,exp]
        js ty' =
          JsNew (JsBuiltIn "Monad")
                [translate method context ty' exp]

-- | Get a JS-representation of a fundamental type for encoding/decoding.
typeRep :: SerializeContext -> FundamentalType -> JsExp
typeRep context typ = case typ of
  FunctionType xs     -> JsList [JsLit $ JsStr "function",JsList (map (typeRep context) xs)]
  JsType x            -> JsList [JsLit $ JsStr "action",JsList [typeRep context x]]
  ListType x          -> JsList [JsLit $ JsStr "list",JsList [typeRep context x]]
  TupleType xs        -> JsList [JsLit $ JsStr "tuple",JsList (map (typeRep context) xs)]
  UserDefined name xs -> JsList [JsLit $ JsStr "user"
                                ,JsLit $ JsStr (unname name)
                                ,JsList (zipWith (\t i -> typeRep (setArg i context) t) xs [0..])]
  Defined x           -> JsList [JsLit $ JsStr "defined",JsList [typeRep context x]]
  Nullable x          -> JsList [JsLit $ JsStr "nullable",JsList [typeRep context x]]
  _ -> nom

  where
    setArg i SerializeUserArg{}   = SerializeUserArg i
    setArg _ c = c
    ret = JsList . return . JsLit . JsStr
    nom = case typ of
      StringType -> ret "string"
      DoubleType -> ret "double"
      PtrType    -> ret "ptr"
      Automatic  -> ret "automatic"
      IntType    -> ret "int"
      BoolType   -> ret "bool"
      DateType   -> ret "date"
      _          ->
        case context of
          SerializeAnywhere -> ret "unknown"
          SerializeUserArg i ->
            let args = JsName argTypes
                automatic = JsIndex 0 (JsName JsParametrizedType)
                thisArg = JsIndex i args
            in JsTernaryIf (JsInfix "&&" args thisArg)
                           thisArg
                           (JsTernaryIf (JsEq automatic (JsLit "automatic"))
                                        (ret "automatic")
                                        (ret "unknown"))

-- | Get the arity of a type.
typeArity :: Type a -> Int
typeArity t = case t of
  TyForall _ _ _ i -> typeArity i
  TyFun _ _ b      -> 1 + typeArity b
  TyParen _ st     -> typeArity st
  _              -> 0

-- | Format the FFI  format string with the given arguments.
formatFFI :: SrcSpanInfo                -- ^ Source Location.
          -> String                     -- ^ The format string.
          -> [(JsName,FundamentalType)] -- ^ Arguments.
          -> Compile String             -- ^ The JS code.
formatFFI loc formatstr args = go formatstr where
  go ('%':'*':xs) = do
    these <- mapM inject (zipWith const [1..] args)
    rest <- go xs
    return (intercalate "," these ++ rest)
  go ('%':'%':xs) = do
    rest <- go xs
    return ('%' : rest)
  go ['%'] = throwError (FfiFormatIncompleteArg loc)
  go ('%':(span isDigit -> (op,xs))) =
    case readMay op of
     Nothing -> throwError (FfiFormatBadChars loc op)
     Just n -> do
       this <- inject n
       rest <- go xs
       return (this ++ rest)
  go (x:xs) = do rest <- go xs
                 return (x : rest)
  go [] = return []

  inject n =
    case listToMaybe (drop (n-1) args) of
      Nothing -> throwError (FfiFormatNoSuchArg loc n)
      Just (arg,typ) ->
        return (printJSString (fayToJs SerializeAnywhere typ (JsName arg)))

-- | Generate n name-typ pairs from the given list.
explodeFields :: [([a], t)] -> [(a, t)]
explodeFields = concatMap $ \(names,typ) -> map (,typ) names

-- | Generate Fay→JS encoding.
fayToJsHash :: [(String, JsExp)] -> [JsStmt]
fayToJsHash cases = [JsExpStmt $ JsApp (JsName $ JsBuiltIn "objConcat") [JsName $ JsBuiltIn "fayToJsHash", JsObj cases]]

-- | Generate JS→Fay decoding.
jsToFayHash :: [(String, JsExp)] -> [JsStmt]
jsToFayHash cases = [JsExpStmt $ JsApp (JsName $ JsBuiltIn "objConcat") [JsName $ JsBuiltIn "jsToFayHash", JsObj cases]]

-- | Make a JS→Fay decoder.
emitJsToFay :: Name a -> [TyVarBind b] -> [([Name c],BangType d)] -> Compile ()
emitJsToFay (unAnn -> name) (map unAnn -> tyvars) (map (unAnn *** unAnn) . explodeFields -> fieldTypes) = do
  qname <- qualify name
  tell (mempty { writerJsToFay = [(printJSString (unAnn name), translator qname)] })

  where
    translator qname =
      JsFun Nothing [JsNameVar "type", argTypes, transcodingObj] []
            (Just $ JsNew (JsConstructor qname)
                          (map (decodeField . getIndex name tyvars) fieldTypes))
    -- Decode JS→Fay field
    decodeField :: (Int,(N.Name,N.BangType)) -> JsExp
    decodeField (i,(fname,typ)) =
      jsToFay (SerializeUserArg i)
              (argType (bangType typ))
              (JsGetPropExtern (JsName transcodingObj)
                               (prettyPrint fname))

-- | The argument types used in serialization of parametrized user-defined types.
argTypes :: JsName
argTypes = JsNameVar "argTypes"

-- | Get the index of a name from the set of type variables bindings.
getIndex :: Name a -> [TyVarBind b] -> (Name c,BangType d) -> (Int,(N.Name,N.BangType))
getIndex (unAnn -> name) (map unAnn -> tyvars) (unAnn -> sname,unAnn -> ty) =
  case bangType ty of
    TyVar _ tyname -> case elemIndex tyname (map tyvar tyvars) of
      Nothing -> error $ "unknown type variable " ++ prettyPrint tyname ++
                         " for " ++ prettyPrint name ++ "." ++ prettyPrint sname ++ "," ++
                         " vars were: " ++ unwords (map prettyPrint tyvars) ++ ", rest: " ++ show tyvars
      Just i -> (i,(sname,ty))
    _ -> (0,(sname,ty))

-- | Extract the name from a possibly-kinded tyvar.
tyvar :: N.TyVarBind -> N.Name
tyvar (UnkindedVar _ v) = v
tyvar (KindedVar _ v _) = v
