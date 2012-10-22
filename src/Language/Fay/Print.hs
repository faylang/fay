{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Simple code (non-pretty) printing.
--
-- No clever printing is done here. If you want pretty printing, use a
-- JS pretty printer. The output should be passed directly to a JS
-- compressor, anyway.
--
-- Special constructors and symbols in Haskell are encoded to
-- JavaScript appropriately.

module Language.Fay.Print where

import           Language.Fay.Types

import           Control.Monad
import           Control.Monad.State
import           Data.Aeson.Encode
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Default
import           Data.List
import           Data.String
import           Language.Haskell.Exts.Syntax
import           Prelude                      hiding (exp)

--------------------------------------------------------------------------------
-- Printing

printJSString :: Printable a => a -> String
printJSString x = concat $ reverse $ psOutput $ execState (runPrinter (printJS x)) def

-- | Print literals. These need some special encoding for
-- JS-format literals. Could use the Text.JSON library.
instance Printable JsLit where
  printJS typ = write $
    let u8 = UTF8.toString . encode . UTF8.fromString
    in case typ of
      (JsChar char)    -> u8 [char]
      (JsStr str)      -> u8 str
      (JsInt int)      -> show int
      (JsFloating rat) -> show rat
      (JsBool b)       -> if b then "true" else "false"

-- | Print (and properly encode to JS) a qualified name.
instance Printable QName where
  printJS qname =
    case qname of
      Qual moduleName name -> moduleName +> "$$" +> name
      UnQual name -> printJS name
      Special con -> printJS con

-- | Print special constructors (tuples, list, etc.)
instance Printable SpecialCon where
  printJS specialCon =
    printJS $ (Qual (ModuleName "Fay") . Ident) $
      case specialCon of
        UnitCon          -> "unit"
        ListCon          -> "emptyList"
        FunCon           -> "funCon"
        TupleCon boxed n -> (if boxed == Boxed
                                then "boxed"
                                else "unboxed" ++
                                "TupleOf" ++ show n)
        Cons             -> "cons"
        UnboxedSingleCon -> "unboxedSingleCon"

-- | Print module name.
instance Printable ModuleName where
  printJS (ModuleName moduleName) =
    write $ jsEncodeName moduleName

-- | Print (and properly encode) a name.
instance Printable Name where
  printJS name = write $
    case name of
      Ident ident -> jsEncodeName ident
      Symbol sym -> jsEncodeName sym

-- | Print a list of statements.
instance Printable [JsStmt] where
  printJS = mapM_ printJS

-- | Print a single statement.
instance Printable JsStmt where
  printJS (JsBlock stmts) =
    "{ " +> stmts +> "}"
  printJS (JsVar name expr) =
    "var " +> name +> " = " +> expr +> ";"
  printJS (JsUpdate name expr) =
    name +> " = " +> expr +> ";"
  printJS (JsSetProp name prop expr) =
    name +> "." +> prop +> " = " +> expr +> ";"
  printJS (JsIf exp thens elses) =
    "if (" +> exp +> ") {" +> thens +> "}" +>
    (when (length elses > 0) $ " else {" +> elses +> "}")
  printJS (JsEarlyReturn exp) =
    "return " +> exp +> ";"
  printJS (JsThrow exp) = do
    "throw " +> exp +> ";"
  printJS (JsWhile cond stmts) =
    "while (" +> cond +> ") {" +> stmts +> "}"
  printJS JsContinue =
    printJS "continue;"
  printJS (JsMappedVar _ name expr) =
    "var " +> name +> " = " +> expr +> ";"

-- | Print an expression.
instance Printable JsExp where
  printJS (JsRawExp name) =
    printJS name
  printJS (JsThrowExp exp) =
    "(function(){ throw (" +> exp +> "); })()"
  printJS JsNull =
    printJS "null"
  printJS (JsName name) =
    printJS name
  printJS (JsLit lit) =
    printJS lit
  printJS (JsParen exp) =
    "(" +> exp +> ")"
  printJS (JsList exps) =
    "[" +> intercalateM "," (map printJS exps) +> printJS "]"
  printJS (JsNew name args) =
    "new " +> (JsApp (JsName name) args)
  printJS (JsIndex i exp) =
    "(" +> exp +> ")[" +> show i +> "]"
  printJS (JsEq exp1 exp2) =
    exp1 +> " === " +> exp2
  printJS (JsGetProp exp prop) =
    exp +> "." +> prop
  printJS (JsLookup exp1 exp2) =
    exp1 +> "[" +> exp2 +> "]"
  printJS (JsUpdateProp name prop expr) =
    "(" +> name +> "." +> prop +> " = " +> expr +> ")"
  printJS (JsInfix op x y) =
    x +> " " +> op +> " " +> y
  printJS (JsGetPropExtern exp prop) =
    exp +> "[" +> (JsLit . JsStr) prop +> "]"
  printJS (JsUpdatePropExtern name prop expr) =
    "(" +> name +> "['" +> prop +> "'] = " +> expr +> ")"
  printJS (JsTernaryIf cond conseq alt) =
    cond +> " ? " +> conseq +> " : " +> alt
  printJS (JsInstanceOf exp classname) =
    exp +> " instanceof " +> classname
  printJS (JsObj assoc) =
    "{" +> (intercalateM "," (map cons assoc)) +> "}"
      where cons (key,value) = "\"" +> key +> "\": " +> value
  printJS (JsFun params stmts ret) =
    "function("
    +> (intercalateM "," (map printJS params))
    +> "){"
    +> stmts
    +> case ret of
      Just ret' -> "return " +> ret' +> ";"
      Nothing   -> return ()
    +> "}"
  printJS (JsApp op args) =
    (if isFunc op then JsParen op else op)
    +> "("
    +> (intercalateM "," (map printJS args))
    +> ")"
     where isFunc JsFun{..} = True; isFunc _ = False
  printJS (JsNegApp args) =
      "-" +> printJS args

--------------------------------------------------------------------------------
-- Utilities

-- Words reserved in haskell as well are not needed here:
-- case, class, do, else, if, import, in, let
reservedWords :: [String]
reservedWords = [
  "break", "catch", "const", "continue", "debugger", "delete", "enum", "export",
  "extends", "finally", "for", "function", "global", "implements", "instanceof",
  "interface", "new", "null", "package", "private", "protected", "public", "return",
  "static", "super", "switch", "this", "throw", "try", "typeof", "undefined",
  "var", "void", "while", "window", "with", "yield","true","false"]

-- | Encode a Haskell name to JavaScript.
-- TODO: Fix this hack.
jsEncodeName :: String -> String
-- Special symbols:
jsEncodeName ":tmp" = "$tmp"
jsEncodeName ":thunk" = "$"
jsEncodeName ":this" = "this"
-- jsEncodeName ":return" = "return"
-- Used keywords:
jsEncodeName name
  | "$_" `isPrefixOf` name = normalize name
  | name `elem` reservedWords = "$_" ++ normalize name
-- Anything else.
jsEncodeName name = normalize name

-- | Normalize the given name to JavaScript-valid names.
normalize :: [Char] -> [Char]
normalize name =
  concatMap encodeChar name

  where
    encodeChar c | c `elem` allowed = [c]
                 | otherwise      = escapeChar c
    allowed = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    escapeChar c = "$" ++ charId c ++ "$"
    charId c = show (fromEnum c)

-- |
write :: String -> Printer a
write x = do
  modify $ \s -> s { psOutput = x : psOutput s }
  return (error "Nothing to return for writer string.")

intercalateM :: String -> [Printer a] -> Printer ()
intercalateM _ [] = return ()
intercalateM _ [x] = x >> return ()
intercalateM str (x:xs) = do
  x
  write str
  intercalateM str xs


-- | Helpful for writing qualified symbols (Fay.*).
instance IsString ModuleName where
  fromString = ModuleName

-- | Helpful for writing variable names.
instance IsString JsName where
  fromString = UnQual . Ident

instance Printable String where
  printJS = write

instance Printable (Printer ()) where
  printJS = id

(+>) :: (Printable a, Printable b) => a -> b -> Printer ()
pa +> pb = printJS pa >> printJS pb
