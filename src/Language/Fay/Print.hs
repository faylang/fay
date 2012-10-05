{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
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
      Qual moduleName name -> do printJS moduleName
                                 "$$"
                                 printJS name
      UnQual name -> printJS name
      Special con -> printJS con

-- | Print special constructors (tuples, list, etc.)
instance Printable SpecialCon where
  printJS specialCon =
    printJS $ (Qual "Fay" . Ident) $
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
  printJS (JsBlock stmts) = do
    "{ "; mapM printJS stmts; "}"
  printJS (JsVar name expr) = do "var "; printJS name; " = "; printJS expr; ";"
  printJS (JsUpdate name expr) = do printJS name; " = "; printJS expr; ";"
  printJS (JsSetProp name prop expr) = do
    printJS name; "."; printJS prop; " = "; printJS expr; ";"
  printJS (JsIf exp thens elses) = do
    "if ("; printJS exp; ") {"
    printJS thens
    "}"
    when (length elses > 0) $ do
      " else {"
      printJS elses
      "}"
  printJS (JsEarlyReturn exp) = do
    "return "; printJS exp; ";"
  printJS (JsThrow exp) = do
    "throw "; printJS exp; ";"
  printJS (JsWhile cond stmts) = do
    "while ("; printJS cond; ") {"
    printJS stmts
    "}"
  printJS JsContinue = "continue;"
  printJS (JsMappedVar _ name expr) = do "var "; printJS name; " = "; printJS expr; ";"

-- | Print an expression.
instance Printable JsExp where
  printJS (JsRawExp name) = write name
  printJS (JsThrowExp exp) = do "(function(){ throw ("; printJS exp; "); })()"
  printJS JsNull = "null"
  printJS (JsName name) = printJS name
  printJS (JsLit lit) = printJS lit
  printJS (JsParen exp) = do "("; printJS exp; ")"
  printJS (JsList exps) = do "["; intercalateM "," (map printJS exps); "]"
  printJS (JsNew name args) = do "new "; printJS (JsApp (JsName name) args)
  printJS (JsIndex i exp) = do "("; printJS exp; ")["; write (show i); "]"
  printJS (JsEq exp1 exp2) = do printJS exp1; " === "; printJS exp2
  printJS (JsGetProp exp prop) = do printJS exp; "."; printJS prop
  printJS (JsLookup exp1 exp2) = do printJS exp1; "["; printJS exp2; "]"
  printJS (JsUpdateProp name prop expr) = do
    "("; printJS name; "."; printJS prop; " = "; printJS expr; ")"
  printJS (JsInfix op x y) = do printJS x; " "; write op; " "; printJS y
  printJS (JsGetPropExtern exp prop) = do
    printJS exp; "["; printJS (JsLit (JsStr prop)); "]"
  printJS (JsUpdatePropExtern name prop expr) = do
    "("; printJS name; "['"; printJS prop; "'] = "; printJS expr; ")"
  printJS (JsTernaryIf cond conseq alt) = do
    printJS cond; " ? "; printJS conseq; " : "; printJS alt
  printJS (JsInstanceOf exp classname) = do
    printJS exp; " instanceof "; printJS classname
  printJS (JsObj assoc) = do "{"; intercalateM "," (map cons assoc); "}"
     where cons (key,value) = do "\""; write key; "\": "; printJS value
  printJS (JsFun params stmts ret) = do
    "function("
    intercalateM "," (map printJS params)
    "){"
    printJS stmts
    case ret of
      Just ret' -> do "return "; printJS ret'; ";"
      Nothing   -> return ()
    "}"
  printJS (JsApp op args) = do
    printJS (if isFunc op then JsParen op else op)
    "("
    intercalateM "," (map (printJS) args)
    ")"
     where isFunc JsFun{..} = True; isFunc _ = False

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

-- | For the pretty printer convenience.
instance IsString (Printer a) where
  fromString = write
