{-# OPTIONS -fno-warn-orphans     #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | Code printers. Can be used to produce both pretty and not
-- pretty output.
--
-- Special constructors and symbols in Haskell are encoded to
-- JavaScript appropriately.

module Fay.Compiler.Print where

import           Fay.Compiler.Prelude

import           Fay.Compiler.PrimOp
import           Fay.Types

import           Data.Aeson.Encode
import qualified Data.ByteString.Lazy.UTF8       as UTF8
import           Language.Haskell.Exts.Annotated hiding (alt, name, op, sym)

--------------------------------------------------------------------------------
-- Printing

-- | Print the JS to a flat string.
printJSString :: Printable a => a -> String
printJSString x = pwOutputString (execPrinter (printJS x) defaultPrintReader)

-- | Print the JS to a pretty string.
printJSPretty :: Printable a => a -> String
printJSPretty x = pwOutputString (execPrinter (printJS x) defaultPrintReader{ prPretty = True })

-- | Encode String to JS-format lterals. Could use the
-- Text.JSON library.
toJsStringLit :: String -> String
toJsStringLit = UTF8.toString . encode

-- | Print literals.
instance Printable JsLit where
  printJS typ = write $ case typ of
      (JsChar char)    -> toJsStringLit [char]
      (JsStr str)      -> toJsStringLit str
      (JsInt int)      -> show int
      (JsFloating rat) -> show rat
      (JsBool b)       -> if b then "true" else "false"

-- | Print (and properly encode to JS) a qualified name.
instance Printable (QName l) where
  printJS qname =
    case qname of
      Qual _ (ModuleName _ "Fay$") name -> "Fay$$" <> printJS name
      Qual _ moduleName name -> printJS moduleName <> printProp name
      UnQual _ name -> printJS name
      Special _ con -> printJS con

-- | Prints pretty operators.
-- prPrettyOperator flag determines the way of accessing operators (e.g. `($)`) and
-- identifiers with apostrophes (e.g. `length'`). If prPrettyOperators is set true,
-- then these will be accessed with square brackets (e.g. Prelude["$"] or
-- Prelude["length'"]). Otherwise special characters will be escaped and accessed
-- with dot (e.g. Prelude.$36$ or Prelude.length$39$). Alphanumeric_ identifiers are
-- always accessed with dot operator (e.g. Prelude.length)
printProp :: Name l -> Printer
printProp name = askIf prPrettyOperators pretty ugly
  where pretty = if all (`elem` allowedNameChars) nameString then dot else brackets
        ugly = dot
        dot = "." <> printJS name
        brackets = "[" <> write (toJsStringLit nameString) <> "]"
        nameString = case name of
          Ident _ s  -> s
          Symbol _ s -> s

-- | Print module name.
instance Printable (ModuleName l) where
  printJS (ModuleName _ "Fay$") = "Fay$"
  printJS (ModuleName _ moduleName) = write $ go moduleName
    where go ('.':xs) = '.' : go xs
          go (x:xs) = normalizeName [x] ++ go xs
          go [] = []

-- | Print (and properly encode) a name.
instance Printable (Name l) where
  printJS = write . encodeName


-- | Print special constructors (tuples, list, etc.)
instance Printable (SpecialCon l) where
  printJS specialCon =
    printJS $ fayBuiltin () $
      case specialCon of
        UnitCon _ -> "unit"
        Cons    _ -> "cons"
        _         -> error $ "Special constructor not supported: " ++
                   show (fmap (const ()) specialCon)


-- | Print a list of statements.
printStmts :: [JsStmt] -> Printer
printStmts = mconcat . map printJS

-- | Print a single statement.
instance Printable JsStmt where
  printJS (JsExpStmt e) =
    printJS e <> ";" <> newline
  printJS (JsBlock stmts) =
    "{ " <> (printStmts stmts) <> "}"
  printJS (JsVar name expr) =
    "var " <> printJS name <> " = " <> printJS expr <> ";" <> newline
  printJS (JsUpdate name expr) =
    printJS name <> " = " <> printJS expr <> ";" <> newline
  printJS (JsSetProp name prop expr) =
    printJS name <> "." <> printJS prop <> " = " <> printJS expr <> ";" <> newline
  printJS (JsSetQName msrcloc name expr) =
    maybe mempty mapping msrcloc <> printJS name <> " = " <> printJS expr <> ";" <> newline
  printJS (JsSetConstructor name expr) =
    printCons name <> " = " <> printJS expr <> ";" <> newline <>
    -- The unqualifiedness here is bad.
    printCons name <> ".prototype.instance = \"" <> printConsUnQual name <> "\";" <> newline
  printJS (JsSetModule mp expr) =
    printJS mp <> " = " <> printJS expr <> ";" <> newline
  printJS (JsSetPropExtern name prop expr) =
    printJS name <> "['" <> printJS prop <> "'] = " <> printJS expr <> ";" <> newline
  printJS (JsIf expr thens elses) =
    "if (" <> printJS expr <> ") {" <> newline <>
    indented (printStmts thens) <>
    "}" <>
    (if (null elses)
      then mempty
      else " else {" <> newline <>
           indented (printStmts elses) <>
           "}") <> newline
  printJS (JsEarlyReturn expr) =
    "return " <> printJS expr <> ";" <> newline
  printJS (JsThrow expr) =
    "throw " <> printJS expr <> ";" <> newline
  printJS (JsWhile cond stmts) =
    "while (" <> printJS cond <> ") {" <> newline <>
    indented (printStmts stmts) <>
    "}" <> newline
  printJS JsContinue = "continue;" <> newline

-- | Print a module path.
instance Printable ModulePath where
  printJS = write . intercalate "." . unModulePath

-- | Print an expression.
instance Printable JsExp where
  printJS (JsSeq es) = "(" <> mintercalate "," (map printJS es) <> ")"
  printJS (JsRawExp e) = write e
  printJS (JsName name) = printJS name
  printJS (JsThrowExp expr) = "(function(){ throw (" <> printJS expr <> "); })()"
  printJS JsNull = "null"
  printJS JsUndefined = "undefined"
  printJS (JsLit lit) = printJS lit
  printJS (JsParen expr) = "(" <> printJS expr <> ")"
  printJS (JsList exprs) = "[" <> mintercalate "," (map printJS exprs) <> "]"
  printJS (JsNew name args) = "new " <> (printJS $ JsApp (JsName name) args)
  printJS (JsIndex i expr) = "(" <> printJS expr <> ")[" <> write (show i) <> "]"
  printJS (JsEq expr1 expr2) = printJS expr1 <> " === " <> printJS expr2
  printJS (JsNeq expr1 expr2) = printJS expr1 <> " !== " <> printJS expr2
  printJS (JsGetProp expr prop) = printJS expr <> "." <> printJS prop
  printJS (JsLookup expr1 expr2) = printJS expr1 <> "[" <> printJS expr2 <> "]"
  printJS (JsUpdateProp name prop expr) =
    "(" <> printJS name <> "." <> printJS prop <> " = " <> printJS expr <> ")"
  printJS (JsInfix op x y) = printJS x <> " " <> write op <> " " <> printJS y
  printJS (JsGetPropExtern expr prop) =
    printJS expr <> "[" <> write (toJsStringLit prop) <> "]"
  printJS (JsUpdatePropExtern name prop expr) =
    "(" <> printJS name <> "['" <> printJS prop <> "'] = " <> printJS expr <> ")"
  printJS (JsTernaryIf cond conseq alt) =
    printJS cond <> " ? " <> printJS conseq <> " : " <> printJS alt
  printJS (JsInstanceOf expr classname) =
    printJS expr <> " instanceof " <> printJS classname
  printJS (JsObj assoc) =
    "{" <> mintercalate "," (map cons assoc) <> "}"
      where cons (key,value) = write (toJsStringLit key) <> ": " <> printJS value
  printJS (JsLitObj assoc) = "{" <> mintercalate "," (map cons assoc) <> "}"
      where cons (key,value) = "\"" <> printJS key <> ": " <> printJS value
  printJS (JsFun nm params stmts ret) =
       "function"
    <> maybe mempty ((" " <>) . printJS . ident) nm
    <> "("
    <> mintercalate "," (map printJS params)
    <> "){" <> newline
    <> indented (printStmts stmts <>
                 case ret of
                   Just ret' -> "return " <> printJS ret' <> ";" <> newline
                   Nothing   -> mempty)
    <> "}"
  printJS (JsApp op args) =
    printJS (case op of JsFun _ _ _ _ -> JsParen op; _ -> op)
    <> "("
    <> mintercalate "," (map printJS args)
    <> ")"
  printJS (JsNegApp args) = "(-(" <> printJS args <> "))"
  printJS (JsAnd a b) = printJS a <> "&&" <> printJS b
  printJS (JsOr a b) = printJS a <> "||" <> printJS b

-- | Unqualify a JsName.
ident :: JsName -> JsName
ident n = case n of
  JsConstructor (Qual _ _ s) -> JsNameVar $ UnQual () s
  a                          -> a

-- | Print one of the kinds of names.
instance Printable JsName where
  printJS name =
    case name of
      JsNameVar qname     -> printJS qname
      JsThis              -> "this"
      JsThunk             -> askIf prPrettyThunks "$" "Fay$$$"
      JsForce             -> askIf prPrettyThunks "_" "Fay$$_"
      JsApply             -> askIf prPrettyThunks "__" "Fay$$__"
      JsParam i           -> "$p" <> (write $ show i)
      JsTmp i             -> "$tmp" <> (write $ show i)
      JsConstructor qname -> printCons qname
      JsBuiltIn qname     -> "Fay$$" <> printJS qname
      JsParametrizedType  -> "type"
      JsModuleName (ModuleName _ m) -> write m

-- | Print a constructor name given a QName.
printCons :: QName l -> Printer
printCons (UnQual _ n) = printConsName n
printCons (Qual _ (ModuleName _ m) n) = write m <> "." <> printConsName n
printCons (Special {}) = error "qname2String Special"

-- | Print an unqualified name.
printConsUnQual :: QName l -> Printer
printConsUnQual (UnQual _ x) = printJS x
printConsUnQual (Qual _ _ n) = printJS n
printConsUnQual (Special {}) = error "printConsUnqual Special"

-- | Print a constructor name given a Name. Helper for printCons.
printConsName :: Name l -> Printer
printConsName = ("_" <>) . printJS

--------------------------------------------------------------------------------
-- Name encoding

-- | Words reserved in haskell as well are not needed here:
-- case, class, do, else, if, import, in, let
reservedWords :: [String]
reservedWords =
  ["abstract","boolean","break","byte","case","catch","char","class"
  ,"comment","const","continue","debugger","default","delete","do","double"
  ,"else","enum","export","extends","false","final","finally","float"
  ,"for","function","global","goto","if","implements","import","in"
  ,"instanceOf","instanceof","int","interface","label","long","native"
  ,"new","null","package","private","protected","public","return","short"
  ,"static","super","switch","synchronized","this","throw","throws"
  ,"transient","true","try","typeof","undefined","var","void","while"
  ,"window","with","yield"]
  -- These are not reserved, but they exist on thunks (inherited from Object) meaning they shouldn't be overridden.
  -- The problem only occurs if there is a module A.B and a constructor B in module A.
   ++ ["__defineGetter__", "__defineSetter__", "__lookupGetter__", "__lookupSetter__", "constructor", "force", "forced", "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable", "toLocaleString", "toString", "value", "valueOf"]

allowedNameChars :: [Char]
allowedNameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

-- | Encode a Haskell name to JavaScript.
encodeName :: Name l -> String

-- | This is a hack for names generated in the Haskell AST. Should be
-- removed once it's no longer needed.
encodeName n = case n of
                 (Ident  _ idn) -> encodeString idn
                 (Symbol _ sym) -> encodeString sym
  where encodeString ('$':'g':'e':'n':name) = "$gen_" ++ normalizeName name
        encodeString name
          | name `elem` reservedWords = "$_" ++ normalizeName name
          | otherwise                 = normalizeName name

-- | Normalize the given name to JavaScript-valid names.
normalizeName :: String -> String
normalizeName = concatMap encodeChar
  where
    encodeChar c | c `elem` allowedNameChars = [c]
                 | otherwise                 = escapeChar c
    escapeChar c = "$" ++ charId c ++ "$"
    charId c = show (fromEnum c)

-- | Intercalate monoids.
mintercalate :: String -> [Printer] -> Printer
mintercalate str xs = mconcat $ intersperse (write str) xs
