{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -Wall #-}

-- | Compiling the FFI support.

module Language.Fay.Compiler.FFI
  (emitFayToJs
  ,emitJsToFay
  ,compileFFI
  ,jsToFayDispatcher
  ,fayToJsDispatcher)
  where

import           Language.Fay.Compiler.Misc
import           Language.Fay.Print           (printJSString)
import           Language.Fay.Types

import           Control.Monad.Error
import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Language.ECMAScript3.Parser  as JS
import           Language.Haskell.Exts        (prettyPrint)
import           Language.Haskell.Exts.Syntax
import           Prelude                      hiding (exp)
import           Safe

-- | Compile an FFI call.
compileFFI :: SrcLoc -- ^ Location of the original FFI decl.
           -> Name  -- ^ Name of the to-be binding.
           -> String -- ^ The format string.
           -> Type   -- ^ Type signature.
           -> Compile [JsStmt]
compileFFI srcloc name formatstr sig = do
  inner <- formatFFI formatstr (zip params funcFundamentalTypes)
  case JS.parse JS.parseExpression (prettyPrint name) (printJSString (wrapReturn inner)) of
    Left err -> throwError (FfiFormatInvalidJavaScript inner (show err))
    Right{}  -> fmap return (bindToplevel srcloc True name (body inner))

  where body inner = foldr wrapParam (wrapReturn inner) params
        wrapParam pname inner = JsFun [pname] [] (Just inner)
        params = zipWith const uniqueNames [1..typeArity sig]
        wrapReturn inner = thunk $
          case lastMay funcFundamentalTypes of
            -- Returns a “pure” value;
            Just{} -> jsToFay returnType (JsRawExp inner)
            -- Base case:
            Nothing -> JsRawExp inner
        funcFundamentalTypes = functionTypeArgs sig
        returnType = last funcFundamentalTypes

-- Make a Fay→JS encoder.
emitFayToJs :: Name -> [([Name],BangType)] -> Compile ()
emitFayToJs name (explodeFields -> fieldTypes) = do
  qname <- qualify name
  modify $ \s -> s { stateFayToJs = translator qname : stateFayToJs s }

  where
    translator qname =
      JsIf (JsInstanceOf (JsName transcodingObjForced) (JsConstructor qname))
           (obj : fieldStmts fieldTypes ++ [ret])
           []

    obj :: JsStmt
    obj = JsVar obj_ $
      JsObj [("instance",JsLit (JsStr (printJSString name)))]

    fieldStmts :: [(Name,BangType)] -> [JsStmt]
    fieldStmts [] = []
    fieldStmts (fieldType:fts) =
      (JsVar obj_v field) :
        (JsIf (JsNeq JsUndefined (JsName obj_v))
          [JsSetPropExtern obj_ decl (JsName obj_v)]
          []) :
        fieldStmts fts
      where
        obj_v = JsNameVar (UnQual (Ident $ "obj_" ++ d))
        decl = JsNameVar (UnQual (Ident d))
        (d, field) = declField fieldType

    obj_ = JsNameVar (UnQual (Ident "obj_"))

    ret :: JsStmt
    ret = JsEarlyReturn (JsName obj_)

    -- Declare/encode Fay→JS field
    declField :: (Name,BangType) -> (String,JsExp)
    declField (fname,typ) =
      (prettyPrint fname
      ,fayToJs (case argType (bangType typ) of
                 known -> typeRep known)
               (force (JsGetProp (JsName transcodingObjForced)
                                 (JsNameVar (UnQual fname)))))

transcodingObj :: JsName
transcodingObj = JsNameVar "obj"

transcodingObjForced :: JsName
transcodingObjForced = JsNameVar "_obj"

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
    TyApp (TyCon "Defined") a -> Defined (argType a)
    TyApp (TyCon "Fay") a -> JsType (argType a)
    TyFun x xs            -> FunctionType (argType x : functionTypeArgs xs)
    TyList x              -> ListType (argType x)
    TyTuple _ xs          -> TupleType (map argType xs)
    TyParen st            -> argType st
    TyApp op arg          -> userDefined (reverse (arg : expandApp op))
    _                     ->
      -- No semantic point to this, merely to avoid GHC's broken
      -- warning.
      case t of
        TyCon (UnQual user)   -> UserDefined user []
        _ -> UnknownType

-- | Extract the type.
bangType :: BangType -> Type
bangType typ =
  case typ of
    BangedTy ty   -> ty
    UnBangedTy ty -> ty
    UnpackedTy ty -> ty

-- | Expand a type application.
expandApp :: Type -> [Type]
expandApp (TyParen t) = expandApp t
expandApp (TyApp op arg) = arg : expandApp op
expandApp x = [x]

-- | Generate a user-defined type.
userDefined :: [Type] -> FundamentalType
userDefined (TyCon (UnQual name):typs) = UserDefined name (map argType typs)
userDefined _ = UnknownType

-- | Translate: JS → Fay.
jsToFay :: FundamentalType -> JsExp -> JsExp
jsToFay typ exp = JsApp (JsName (JsBuiltIn "jsToFay"))
                        [typeRep typ,exp]

-- | Translate: Fay → JS.
fayToJs :: JsExp -> JsExp -> JsExp
fayToJs typ exp = JsApp (JsName (JsBuiltIn "fayToJs"))
                        [typ,exp]

-- | Get a JS-representation of a fundamental type for encoding/decoding.
typeRep :: FundamentalType -> JsExp
typeRep typ =
  case typ of
    FunctionType xs     -> JsList [JsLit $ JsStr "function",JsList (map typeRep xs)]
    JsType x            -> JsList [JsLit $ JsStr "action",JsList [typeRep x]]
    ListType x          -> JsList [JsLit $ JsStr "list",JsList [typeRep x]]
    TupleType xs        -> JsList [JsLit $ JsStr "tuple",JsList (map typeRep xs)]
    UserDefined name xs -> JsList [JsLit $ JsStr "user"
                                  ,JsLit $ JsStr (unname name)
                                  ,JsList (map typeRep xs)]
    Defined x           -> JsList [JsLit $ JsStr "defined",JsList [typeRep x]]
    _ -> JsList [JsLit $ JsStr nom]

      where nom = case typ of
              StringType -> "string"
              DoubleType -> "double"
              IntType    -> "int"
              BoolType   -> "bool"
              DateType   -> "date"
              _          -> "unknown"

-- | Get the arity of a type.
typeArity :: Type -> Int
typeArity t =
  case t of
    TyForall _ _ i -> typeArity i
    TyFun _ b      -> 1 + typeArity b
    TyParen st     -> typeArity st
    _              -> 0

-- | Format the FFI format string with the given arguments.
formatFFI :: String                      -- ^ The format string.
          -> [(JsName,FundamentalType)] -- ^ Arguments.
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

explodeFields :: [([a], t)] -> [(a, t)]
explodeFields = concatMap $ \(names,typ) -> map (,typ) names

fayToJsDispatcher :: [JsStmt] -> JsStmt
fayToJsDispatcher cases =
  JsVar (JsBuiltIn "fayToJsUserDefined")
        (JsFun [JsNameVar "type",transcodingObj]
               (decl ++ cases ++ [baseCase])
               Nothing)

  where decl = [JsVar transcodingObjForced
                      (force (JsName transcodingObj))
               ,JsVar (JsNameVar "argTypes")
                      (JsLookup (JsName (JsNameVar "type"))
                                (JsLit (JsInt 2)))]
        baseCase =
          JsEarlyReturn (JsName transcodingObj)

jsToFayDispatcher :: [JsStmt] -> JsStmt
jsToFayDispatcher cases =
  JsVar (JsBuiltIn "jsToFayUserDefined")
        (JsFun [JsNameVar "type",transcodingObj]
               (cases ++ [baseCase])
               Nothing)

  where baseCase =
          JsEarlyReturn (JsName transcodingObj)

-- Make a JS→Fay decoder
emitJsToFay ::  Name -> [([Name], BangType)] -> Compile ()
emitJsToFay name (explodeFields -> fieldTypes) = do
  qname <- qualify name
  modify $ \s -> s { stateJsToFay = translator qname : stateJsToFay s }

  where
    translator qname =
      JsIf (JsEq (JsGetPropExtern (JsName transcodingObj) "instance")
                 (JsLit (JsStr (printJSString name))))
           [JsEarlyReturn (JsNew (JsConstructor qname)
                                 (map decodeField fieldTypes))]
           []
    -- Decode JS→Fay field
    decodeField :: (Name,BangType) -> JsExp
    decodeField (fname,typ) =
      jsToFay (argType (bangType typ))
              (JsGetPropExtern (JsName transcodingObj)
                               (prettyPrint fname))
