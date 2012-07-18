{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Simple code (non-pretty) printing.
--
-- No clever printing is done here. If you want pretty printing, use a
-- JS pretty printer. The output should be passed directly to a JS
-- compressor, anyway.
--
-- Special constructors and symbols in Haskell are encoded to
-- JavaScript appropriately.

module Language.Fay.Print where

import Language.Fay.Types

import Data.List
import Data.String
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)
import Text.JSON

--------------------------------------------------------------------------------
-- Printing

-- | Print literals. These need some special encoding for
-- JS-format literals. Could use the Text.JSON library.
instance Printable JsLit where
  printJS (JsChar char)    = encode [char] -- FIXME:
  printJS (JsStr str)      = encode str  -- FIXME:
  printJS (JsInt int)      = show int  -- FIXME:
  printJS (JsFloating rat) = show rat  -- FIXME:
  printJS (JsBool b)       = if b then "true" else "false"

-- | Print (and properly encode to JS) a qualified name.
instance Printable QName where
  printJS qname =
    case qname of
      Qual moduleName name -> printJS moduleName ++ "$$" ++ printJS name
      UnQual name -> printJS name
      Special con -> printJS con

-- | Print special constructors (tuples, list, etc.)
instance Printable SpecialCon where
  printJS specialCon =
    case specialCon of
      UnitCon          -> printJS (Qual "Fay" (Ident "unit"))
      ListCon          -> printJS (Qual "Fay" (Ident "emptyList"))
      FunCon           -> printJS (Qual "Fay" (Ident "funCon"))
      TupleCon boxed n -> printJS (Qual "Fay"
                                         (Ident (if boxed == Boxed
                                                    then "boxed"
                                                    else "unboxed" ++
                                                    "TupleOf" ++ show n)))
      Cons             -> printJS (Qual "Fay" (Ident "cons"))
      UnboxedSingleCon -> printJS (Qual "Fay" (Ident "unboxedSingleCon"))

-- | Print module name.
instance Printable ModuleName where
  printJS (ModuleName moduleName) =
    jsEncodeName moduleName

-- | Print (and properly encode) a name.
instance Printable Name where
  printJS name =
    case name of
      Ident ident -> jsEncodeName ident
      Symbol sym -> jsEncodeName sym

-- | Print a list of statements.
instance Printable [JsStmt] where
  printJS = concat . map (printJS)

-- | Print a single statement.
instance Printable JsStmt where
  printJS (JsVar name expr) =
    (unwords ["var",printJS name,"=",printJS expr ++ ";"])
  printJS (JsIf exp thens elses) =
    concat
      [("if (" ++ printJS exp ++ ") {")
      ,printJS thens] ++
      if (length elses > 0)
         then concat ["} else {"
                     ,printJS elses ++ "}"]
         else "}"
  printJS (JsEarlyReturn exp) =
    ("return (" ++ printJS exp ++ ");")
  printJS (JsThrow exp) =
    ("throw (" ++ printJS exp ++ ");")

-- | Print an expression.
instance Printable JsExp where
  printJS (JsRawName name) = name
  printJS (JsThrowExp exp) =
    "(function(){ throw (" ++ printJS exp ++ "); })()"
  printJS (JsFun params stmts ret) =
    concat ["function("
           ,intercalate "," (map (printJS) params)
           ,"){"
           ,printJS stmts
           ] ++
    case ret of
      Just ret' ->
        concat ["return "
               ,printJS ret'
               ,";"
               ,"}"]
      Nothing -> "}"
  printJS JsNull = "null"
  printJS (JsSequence exprs) =
    intercalate "," (map (printJS) exprs)
  printJS (JsName name) = printJS name
  printJS (JsApp op args) = 
    printJS (if isFunc op then JsParen op else op) ++
    "(" ++ 
    intercalate "," (map (printJS) args) ++
    ")"
     where isFunc JsFun{..} = True; isFunc _ = False
  printJS (JsLit lit) = printJS lit
  printJS (JsParen exp) = "(" ++ printJS exp ++ ")"
  printJS (JsTernaryIf cond conseq alt) =
    concat [printJS cond ++ " ? "
           , (printJS conseq) ++ " : "
           , (printJS alt)]
  printJS (JsList exps) =
    "[" ++ 
    intercalate "," (map (printJS) exps) ++
    "]"
  printJS (JsNew name args) =
    "new " ++ printJS (JsApp (JsName name) args)
  printJS (JsInstanceOf exp classname) =
    printJS exp ++ " instanceof " ++ printJS classname
  printJS (JsIndex i exp) = 
    "(" ++ printJS exp ++ ")[" ++ show i ++ "]"
  printJS (JsEq exp1 exp2) =
    "(" ++ printJS exp1 ++ " == " ++ printJS exp2 ++ ")"
  printJS (JsGetProp exp prop) =
    printJS exp ++ "." ++ printJS prop

--------------------------------------------------------------------------------
-- Utilities

-- | Encode a Haskell name to JavaScript.
jsEncodeName :: String -> String
-- Special symbols:
jsEncodeName ":tmp" = "$tmp"
-- Used keywords:
jsEncodeName "null" = "_null"
-- Anything else.
jsEncodeName name =
  if isPrefixOf "$_" name
     then name
     else concat . map encode $ name
       
  where
    encode c | elem c allowed = [c]
             | otherwise      = escapeChar c
    allowed = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    escapeChar c = "$" ++ charId c ++ "$"
    charId c = show (fromEnum c)

-- | Helpful for writing qualified symbols (Fay.*). 
instance IsString ModuleName where
  fromString = ModuleName

-- | Helpful for writing variable names.
instance IsString JsName where
  fromString = UnQual . Ident
