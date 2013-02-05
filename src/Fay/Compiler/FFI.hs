{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -Wall #-}

-- | Compiling the FFI support.

module Fay.Compiler.FFI
  (emitFayToJs
  ,emitJsToFay
  ,compileFFI
  ,jsToFayDispatcher
  ,fayToJsDispatcher)
  where

import Fay.Compiler.Misc
import Fay.Compiler.Print           (printJSString)
import Fay.Types

import Control.Monad.Error
import Control.Monad.Writer
import Control.Applicative ((<$>), (<*>))
import Data.Char
import Data.Generics.Schemes
import Data.List
import Data.Maybe
import Data.String
import Language.ECMAScript3.Parser  as JS
import Language.ECMAScript3.Syntax
import Language.Haskell.Exts        (prettyPrint)
import Language.Haskell.Exts.Syntax
import Prelude                      hiding (exp)
import Safe

-- | Compile an FFI call.
compileFFI :: SrcLoc -- ^ Location of the original FFI decl.
           -> Name  -- ^ Name of the to-be binding.
           -> String -- ^ The format string.
           -> Type   -- ^ Type signature.
           -> Compile [JsStmt]
  -- substitute newtypes with their child types before calling
  -- real compileFFI
  compileFFI' srcloc name formatstr =<< rmNewtys sig

  where rmNewtys :: Type -> Compile Type
        rmNewtys (TyForall b c t) = TyForall b c <$> rmNewtys t
        rmNewtys (TyFun t1 t2)    = TyFun <$> rmNewtys t1 <*> rmNewtys t2
        rmNewtys (TyTuple b tl)   = TyTuple b <$> mapM rmNewtys tl
        rmNewtys (TyList t)       = TyList <$> rmNewtys t
        rmNewtys (TyApp t1 t2)    = TyApp <$> rmNewtys t1 <*> rmNewtys t2
        rmNewtys t@TyVar{}        = return t
        rmNewtys (TyCon qname)    = do
          newty <- lookupNewtypeConst =<< qualifyQName qname
          return $ case newty of
                     Nothing     -> TyCon qname
                     Just (_,ty) -> ty
        rmNewtys (TyParen t)      = TyParen <$> rmNewtys t
        rmNewtys (TyInfix t1 q t2)= flip TyInfix q <$> rmNewtys t1 <*> rmNewtys t2
        rmNewtys (TyKind t k)     = flip TyKind k <$> rmNewtys t

compileFFI' :: SrcLoc -> Name -> String -> Type -> Compile [JsStmt]
compileFFI' srcloc name formatstr sig = do
  inner <- formatFFI srcloc formatstr (zip params funcFundamentalTypes)
  case JS.parse JS.parseExpression (prettyPrint name) (printJSString (wrapReturn inner)) of
    Left err -> throwError (FfiFormatInvalidJavaScript srcloc inner (show err))
    Right exp  -> do
      config' <- config id
      when (configGClosure config') $ warnDotUses srcloc inner exp
      fmap return (bindToplevel srcloc True name (body inner))

  where body inner = foldr wrapParam (wrapReturn inner) params
        wrapParam pname inner = JsFun [pname] [] (Just inner)
        params = zipWith const uniqueNames [1..typeArity sig]
        wrapReturn inner = thunk $
          case lastMay funcFundamentalTypes of
            -- Returns a “pure” value;
            Just{} -> jsToFay SerializeAnywhere returnType (JsRawExp inner)
            -- Base case:
            Nothing -> JsRawExp inner
        funcFundamentalTypes = functionTypeArgs sig
        returnType = last funcFundamentalTypes

-- | Warn about uses of naked x.y which will not play nicely with Google Closure.
warnDotUses :: SrcLoc -> String -> ParsedExpression -> Compile ()
warnDotUses srcloc string expr =
  when anyrefs $
    warn $ printSrcLoc srcloc ++ ":\nDot ref syntax used in FFI JS code: " ++ string

  where anyrefs = not (null (listify dotref expr)) ||
                  not (null (listify ldot expr))

        dotref :: ParsedExpression -> Bool
        dotref x = case x of
          DotRef _ (VarRef _ (Id _ name)) _
             | elem name globalNames -> False
          DotRef{}                   -> True
          _                          -> False

        ldot :: LValue SourcePos -> Bool
        ldot x =
          case x of
            LDot _ (VarRef _ (Id _ name)) _
             | elem name globalNames -> False
            LDot{}                   -> True
            _                        -> False

        globalNames = ["Math","console","JSON"]

-- | Make a Fay→JS encoder.
emitFayToJs :: Name -> [([Name],BangType)] -> Compile ()
emitFayToJs name (explodeFields -> fieldTypes) = do
  qname <- qualify name
  tell (mempty { writerFayToJs = [translator qname] })

  where
    translator qname =
      JsIf (JsInstanceOf (JsName transcodingObjForced) (JsConstructor qname))
           (obj : fieldStmts (zip [0..] fieldTypes) ++ [ret])
           []

    obj :: JsStmt
    obj = JsVar obj_ $
      JsObj [("instance",JsLit (JsStr (printJSString name)))]

    fieldStmts :: [(Int,(Name,BangType))] -> [JsStmt]
    fieldStmts [] = []
    fieldStmts ((i,fieldType):fts) =
      (JsVar obj_v field) :
        (JsIf (JsNeq JsUndefined (JsName obj_v))
          [JsSetPropExtern obj_ decl (JsName obj_v)]
          []) :
        fieldStmts fts
      where
        obj_v = JsNameVar (UnQual (Ident $ "obj_" ++ d))
        decl = JsNameVar (UnQual (Ident d))
        (d, field) = declField i fieldType

    obj_ = JsNameVar (UnQual (Ident "obj_"))

    ret :: JsStmt
    ret = JsEarlyReturn (JsName obj_)

    -- Declare/encode Fay→JS field
    declField :: Int -> (Name,BangType) -> (String,JsExp)
    declField i (fname,typ) =
      (prettyPrint fname
      ,fayToJs (SerializeUserArg i)
               (argType (bangType typ))
               (JsGetProp (JsName transcodingObjForced)
                          (JsNameVar (UnQual fname))))

-- | A name used for transcoding.
transcodingObj :: JsName
transcodingObj = JsNameVar "obj"

-- | The name used for the forced version of a transcoding variable.
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
argType t = case t of
  TyCon "String"              -> StringType
  TyCon "Double"              -> DoubleType
  TyCon "Int"                 -> IntType
  TyCon "Bool"                -> BoolType
  TyApp (TyCon "Ptr") _       -> PtrType
  TyApp (TyCon "Automatic") _ -> Automatic
  TyApp (TyCon "Defined") a   -> Defined (argType a)
  TyApp (TyCon "Nullable") a  -> Nullable (argType a)
  TyApp (TyCon "Fay") a       -> JsType (argType a)
  TyFun x xs                  -> FunctionType (argType x : functionTypeArgs xs)
  TyList x                    -> ListType (argType x)
  TyTuple _ xs                -> TupleType (map argType xs)
  TyParen st                  -> argType st
  TyApp op arg                -> userDefined (reverse (arg : expandApp op))
  _                     ->
    -- No semantic point to this, merely to avoid GHC's broken
    -- warning.
    case t of
      TyCon (UnQual user)   -> UserDefined user []
      _ -> UnknownType

-- | Extract the type.
bangType :: BangType -> Type
bangType typ = case typ of
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
          JsApp (JsName (JsBuiltIn (fromString (method ++ "_" ++ specialize))))
                [exp]
        recursive =
          JsApp (JsName (JsBuiltIn (fromString method)))
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
typeArity :: Type -> Int
typeArity t = case t of
  TyForall _ _ i -> typeArity i
  TyFun _ b      -> 1 + typeArity b
  TyParen st     -> typeArity st
  _              -> 0

-- | Format the FFI format string with the given arguments.
formatFFI :: SrcLoc                     -- ^ Location of the original FFI decl.
          -> String                     -- ^ The format string.
          -> [(JsName,FundamentalType)] -- ^ Arguments.
          -> Compile String             -- ^ The JS code.
formatFFI srcloc formatstr args = go formatstr where
  go ('%':'*':xs) = do
    these <- mapM inject (zipWith const [1..] args)
    rest <- go xs
    return (intercalate "," these ++ rest)
  go ('%':'%':xs) = do
    rest <- go xs
    return ('%' : rest)
  go ['%'] = throwError (FfiFormatIncompleteArg srcloc)
  go ('%':(span isDigit -> (op,xs))) =
    case readMay op of
     Nothing -> throwError (FfiFormatBadChars srcloc op)
     Just n -> do
       this <- inject n
       rest <- go xs
       return (this ++ rest)
  go (x:xs) = do rest <- go xs
                 return (x : rest)
  go [] = return []

  inject n =
    case listToMaybe (drop (n-1) args) of
      Nothing -> throwError (FfiFormatNoSuchArg srcloc n)
      Just (arg,typ) -> do
        return (printJSString (fayToJs SerializeAnywhere typ (JsName arg)))

-- | Generate n name-typ pairs from the given list.
explodeFields :: [([a], t)] -> [(a, t)]
explodeFields = concatMap $ \(names,typ) -> map (,typ) names

-- | The dispatcher for Fay->JS conversion.
fayToJsDispatcher :: [JsStmt] -> JsStmt
fayToJsDispatcher cases =
  JsVar (JsBuiltIn "fayToJsUserDefined")
        (JsFun [JsNameVar "type",transcodingObj]
               (decl ++ cases ++ [baseCase])
               Nothing)

  where decl = [JsVar transcodingObjForced
                      (force (JsName transcodingObj))
               ,JsVar argTypes
                      (JsLookup (JsName JsParametrizedType)
                                (JsLit (JsInt 2)))]
        baseCase =
          JsEarlyReturn (JsName transcodingObj)

-- | The dispatcher for JS->Fay conversion.
jsToFayDispatcher :: [JsStmt] -> JsStmt
jsToFayDispatcher cases =
  JsVar (JsBuiltIn "jsToFayUserDefined")
        (JsFun [JsNameVar "type",transcodingObj]
               (decl ++ cases ++ [baseCase])
               Nothing)

  where baseCase =
          JsEarlyReturn (JsName transcodingObj)
        decl = [JsVar argTypes
                      (JsLookup (JsName JsParametrizedType)
                                (JsLit (JsInt 2)))]

-- | Make a JS→Fay decoder.
emitJsToFay ::  Name -> [([Name], BangType)] -> Compile ()
emitJsToFay name (explodeFields -> fieldTypes) = do
  qname <- qualify name
  tell (mempty { writerJsToFay = [translator qname] })

  where
    translator qname =
      JsIf (JsEq (JsGetPropExtern (JsName transcodingObj) "instance")
                 (JsLit (JsStr (printJSString name))))
           [JsEarlyReturn (JsNew (JsConstructor qname)
                                 (zipWith decodeField fieldTypes [0..]))]
           []
    -- Decode JS→Fay field
    decodeField :: (Name,BangType) -> Int -> JsExp
    decodeField (fname,typ) i =
      jsToFay (SerializeUserArg i)
              (argType (bangType typ))
              (JsGetPropExtern (JsName transcodingObj)
                               (prettyPrint fname))

-- | The argument types used in serialization of parametrized user-defined types.
argTypes :: JsName
argTypes = JsNameVar "argTypes"
