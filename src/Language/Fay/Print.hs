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
import qualified Data.ByteString.Lazy.UTF8    as UTF8
import           Data.Default
import           Data.List
import           Data.String
import           Language.Haskell.Exts.Syntax

import           Prelude                      hiding (exp)

--------------------------------------------------------------------------------
-- Printing

printJSString :: Printable a => a -> String
printJSString x = concat $ reverse $ psOutput $ execState (runPrinter (printJS x)) def

printJSPretty :: Printable a => a -> String
printJSPretty x = concat $ reverse $ psOutput $ execState (runPrinter (printJS x)) def { psPretty = True }

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
      Qual moduleName name -> moduleName +> "$" +> name
      UnQual name -> printJS name
      Special con -> printJS con

-- | Print module name.
instance Printable ModuleName where
  printJS (ModuleName "Fay$") =
    write "Fay$"
  printJS (ModuleName moduleName) = write $ go moduleName

    where go ('.':xs) = '$' : go xs
          go (x:xs) = normalizeName [x] ++ go xs
          go [] = []

-- | Print special constructors (tuples, list, etc.)
instance Printable SpecialCon where
  printJS specialCon =
    printJS $ (Qual (ModuleName "Fay$") . Ident) $
      case specialCon of
        UnitCon -> "unit"
        Cons    -> "cons"
        _       -> error $ "Special constructor not supported: " ++ show specialCon

-- | Print (and properly encode) a name.
instance Printable Name where
  printJS name = write $
    case name of
      Ident ident -> encodeName ident
      Symbol sym -> encodeName sym

-- | Print a list of statements.
instance Printable [JsStmt] where
  printJS = mapM_ printJS

-- | Print a single statement.
instance Printable JsStmt where
  printJS (JsBlock stmts) =
    "{ " +> stmts +> "}"
  printJS (JsVar name expr) =
    "var " +> name +> " = " +> expr +> ";" +> newline
  printJS (JsUpdate name expr) =
    name +> " = " +> expr +> ";" +> newline
  printJS (JsSetProp name prop expr) =
    name +> "." +> prop +> " = " +> expr +> ";" +> newline
  printJS (JsSetPropExtern name prop expr) =
    name +> "['" +> prop +> "'] = " +> expr +> ";" +> newline
  printJS (JsIf exp thens elses) =
    "if (" +> exp +> ") {" +> newline +>
    indented (printJS thens) +>
    "}" +>
    (when (length elses > 0) $ " else {" +>
    indented (printJS elses) +>
    "}") +> newline
  printJS (JsEarlyReturn exp) =
    "return " +> exp +> ";" +> newline
  printJS (JsThrow exp) = do
    "throw " +> exp +> ";" +> newline
  printJS (JsWhile cond stmts) =
    "while (" +> cond +> ") {"  +> newline +>
    indented (printJS stmts) +>
    "}" +> newline
  printJS JsContinue =
    printJS "continue;" +> newline
  printJS (JsMappedVar _ name expr) =
    "var " +> name +> " = " +> expr +> ";" +> newline

-- | Print an expression.
instance Printable JsExp where
  printJS (JsRawExp e) = write e
  printJS (JsName name) = printJS name
  printJS (JsThrowExp exp) =
    "(function(){ throw (" +> exp +> "); })()"
  printJS JsNull =
    printJS "null"
  printJS JsUndefined =
    printJS "undefined"
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
  printJS (JsNeq exp1 exp2) =
    exp1 +> " !== " +> exp2
  printJS (JsGetProp exp prop) = exp +> "." +> prop
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
    +> "){" +> newline
    +> indented (stmts +>
                 case ret of
                   Just ret' -> "return " +> ret' +> ";" +> newline
                   Nothing   -> return ())
    +> "}"
  printJS (JsApp op args) =
    (if isFunc op then JsParen op else op)
    +> "("
    +> (intercalateM "," (map printJS args))
    +> ")"
     where isFunc JsFun{..} = True; isFunc _ = False
  printJS (JsNegApp args) =
      "(-(" +> printJS args +> "))"

-- | Print one of the kinds of names.
instance Printable JsName where
  printJS name =
    case name of
      JsNameVar qname     -> printJS qname
      JsThis              -> write "this"
      JsThunk             -> write "Fay$$$"
      JsForce             -> write "Fay$$_"
      JsApply             -> write "Fay$$__"
      JsParam i           -> write ("$p" ++ show i)
      JsTmp i             -> write ("$tmp" ++ show i)
      JsConstructor qname -> "$_" +> printJS qname
      JsBuiltIn qname     -> "Fay$$" +> printJS qname
      JsParametrizedType  -> write "type"

instance Printable String where
  printJS = write

instance Printable (Printer ()) where
  printJS = id

--------------------------------------------------------------------------------
-- Name encoding

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
encodeName :: String -> String
-- | This is a hack for names generated in the Haskell AST. Should be
-- removed once it's no longer needed.
encodeName ('$':'g':'e':'n':name) = "$gen_" ++ normalizeName name
encodeName name
  | name `elem` reservedWords = "$_" ++ normalizeName name
  | otherwise                 = normalizeName name

-- | Normalize the given name to JavaScript-valid names.
normalizeName :: [Char] -> [Char]
normalizeName name =
  concatMap encodeChar name

  where
    encodeChar c | c `elem` allowed = [c]
                 | otherwise        = escapeChar c
    allowed = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    escapeChar c = "$" ++ charId c ++ "$"
    charId c = show (fromEnum c)

--------------------------------------------------------------------------------
-- Printing


-- | Print the given printer indented.
indented :: Printer a -> Printer ()
indented p = do
  PrintState{..} <- get
  if psPretty
     then do modify $ \s -> s { psIndentLevel = psIndentLevel + 1 }
             p
             modify $ \s -> s { psIndentLevel = psIndentLevel }
     else p >> return ()

-- | Output a newline.
newline :: Printer ()
newline = do
  PrintState{..} <- get
  when psPretty $ do
    write "\n"
    modify $ \s -> s { psNewline = True }

-- | Write out a string, updating the current position information.
write :: String -> Printer ()
write x = do
  PrintState{..} <- get
  let out = if psNewline then replicate (2*psIndentLevel) ' ' ++ x else x
  modify $ \s -> s { psOutput  = out : psOutput
                   , psLine    = psLine + additionalLines
                   , psColumn  = if additionalLines > 0
                                    then length (concat (take 1 (reverse srclines)))
                                    else psColumn + length x
                   , psNewline = False
                   }
  return (error "Nothing to return for writer string.")

  where srclines = lines x
        additionalLines = length (filter (=='\n') x)

-- | Intercalate monadic action.
intercalateM :: String -> [Printer a] -> Printer ()
intercalateM _ [] = return ()
intercalateM _ [x] = x >> return ()
intercalateM str (x:xs) = do
  x
  write str
  intercalateM str xs

-- | Concatenate two printables.
(+>) :: (Printable a, Printable b) => a -> b -> Printer ()
pa +> pb = printJS pa >> printJS pb
