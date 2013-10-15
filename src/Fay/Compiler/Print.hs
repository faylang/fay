{-# OPTIONS -fno-warn-orphans        #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Simple code (non-pretty) printing.
--
-- No clever printing is done here. If you want pretty printing, use a
-- JS pretty printer. The output should be passed directly to a JS
-- compressor, anyway.
--
-- Special constructors and symbols in Haskell are encoded to
-- JavaScript appropriately.

module Fay.Compiler.Print where

import           Fay.Compiler.PrimOp
import qualified Fay.Exts.NoAnnotation                  as N
import           Fay.Types

import           Control.Monad
import           Control.Monad.State
import           Data.Aeson.Encode
import qualified Data.ByteString.Lazy.UTF8              as UTF8
import           Data.Default
import           Data.List
import           Data.String
import           Language.Haskell.Exts.Annotated
import           Prelude                                hiding (exp)
import           SourceMap.Types

--------------------------------------------------------------------------------
-- Printing

-- | Print the JS to a flat string.
printJSString :: Printable a => a -> String
printJSString x = concat $ reverse $ psOutput $ execState (runPrinter (printJS x)) def

-- | Print the JS to a pretty string.
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
instance Printable N.QName where
  printJS qname =
    case qname of
      Qual _ (ModuleName _ "Fay$") name -> "Fay$$" +> name
      Qual _ moduleName name -> moduleName +> "." +> name
      UnQual _ name -> printJS name
      Special _ con -> printJS con

-- | Print module name.
instance Printable N.ModuleName where
  printJS (ModuleName _ "Fay$") =
    write "Fay$"
  printJS (ModuleName _ moduleName) = write $ go moduleName

    where go ('.':xs) = '.' : go xs
          go (x:xs) = normalizeName [x] ++ go xs
          go [] = []

-- | Print special constructors (tuples, list, etc.)
instance Printable N.SpecialCon where
  printJS specialCon =
    printJS $ fayBuiltin () $
      case specialCon of
        UnitCon _ -> "unit"
        Cons    _ -> "cons"
        _       -> error $ "Special constructor not supported: " ++ show specialCon

-- | Print (and properly encode) a name.
instance Printable N.Name where
  printJS name = write $
    case name of
      Ident  _ idn -> encodeName idn
      Symbol _ sym -> encodeName sym

-- | Print a list of statements.
instance Printable [JsStmt] where
  printJS = mapM_ printJS

-- | Print a single statement.
instance Printable JsStmt where
  printJS (JsExpStmt e) =
    printJS e +> ";" +> newline
  printJS (JsBlock stmts) =
    "{ " +> stmts +> "}"
  printJS (JsVar name expr) =
    "var " +> name +> " = " +> expr +> ";" +> newline
  printJS (JsUpdate name expr) =
    name +> " = " +> expr +> ";" +> newline
  printJS (JsSetProp name prop expr) =
    name +> "." +> prop +> " = " +> expr +> ";" +> newline
  printJS (JsSetQName msrcloc name expr) = do
    maybe (return ()) mapping msrcloc
    name +> " = " +> expr +> ";" +> newline
  printJS (JsSetConstructor name expr) =
    printCons name +> " = " +> expr +> ";" +> newline +>
    -- The unqualifiedness here is bad.
    printCons name +> ".prototype.instance = \"" +> printConsUnQual name +> "\";" +> newline
  printJS (JsSetModule mp expr) =
    mp +> " = " +> expr +> ";" +> newline
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

-- | Print a module path.
instance Printable ModulePath where
  printJS (unModulePath -> l) = write $ intercalate "." l

-- | Print an expression.
instance Printable JsExp where
  printJS (JsSeq es) = "(" +> intercalateM "," (map printJS es) +> ")"
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
  printJS (JsLitObj assoc) =
    "{" +> (intercalateM "," (map cons assoc)) +> "}"
      where
        cons :: (N.Name, JsExp) -> Printer ()
        cons (key,value) = "\"" +> key +> "\": " +> value
  printJS (JsFun nm params stmts ret) =
       "function"
    +> maybe (return ()) ((" " +>) . printJS . ident) nm
    +> "("
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
  printJS (JsAnd a b) =
      printJS a +> "&&" +> printJS b
  printJS (JsOr a b) =
      printJS a +> "||" +> printJS b

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
      JsThis              -> write "this"
      JsThunk             -> write "Fay$$$"
      JsForce             -> write "Fay$$_"
      JsApply             -> write "Fay$$__"
      JsParam i           -> write ("$p" ++ show i)
      JsTmp i             -> write ("$tmp" ++ show i)
      JsConstructor qname -> printCons qname
      JsBuiltIn qname     -> "Fay$$" +> printJS qname
      JsParametrizedType  -> write "type"
      JsModuleName (ModuleName _ m) -> write m

-- | Print a constructor name given a QName.
printCons :: N.QName -> Printer ()
printCons (UnQual _ n) = printConsName n
printCons (Qual _ (ModuleName _ m) n) = printJS m +> "." +> printConsName n
printCons (Special {}) = error "qname2String Special"

-- | Print an unqualified name.
printConsUnQual :: N.QName -> Printer ()
printConsUnQual (UnQual _ x) = printJS x
printConsUnQual (Qual _ _ n) = printJS n
printConsUnQual (Special {}) = error "printConsUnqual Special"

-- | Print a constructor name given a Name. Helper for printCons.
printConsName :: N.Name -> Printer ()
printConsName n = write "_" >> printJS n

-- | Just write out strings.
instance Printable String where
  printJS = write

-- | A printer is a printable.
instance Printable (Printer ()) where
  printJS = id

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


-- | Encode a Haskell name to JavaScript.
encodeName :: String -> String
-- | This is a hack for names generated in the Haskell AST. Should be
-- removed once it's no longer needed.
encodeName ('$':'g':'e':'n':name) = "$gen_" ++ normalizeName name
encodeName name
  | name `elem` reservedWords = "$_" ++ normalizeName name
  | otherwise                 = normalizeName name

-- | Normalize the given name to JavaScript-valid names.
normalizeName :: String -> String
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

-- | Generate a mapping from the Haskell location to the current point in the output.
mapping :: SrcSpan -> Printer ()
mapping SrcSpan{..} = do
  modify $ \s -> s  { psMappings = m s : psMappings s }
  return ()

  where m ps = Mapping { mapGenerated = Pos (fromIntegral (psLine ps))
                                            (fromIntegral (psColumn ps))
                       , mapOriginal = Just (Pos (fromIntegral srcSpanStartLine)
                                                 (fromIntegral srcSpanStartColumn - 1))
                       , mapSourceFile = Just srcSpanFilename
                       , mapName = Nothing
                       }

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
