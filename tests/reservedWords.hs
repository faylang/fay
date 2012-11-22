{-# LANGUAGE EmptyDataDecls    #-}

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do
  -- All reserved words
  let break = "break" in printS break
  let catch = "catch" in printS catch
  let const = "const" in printS const
  let continue = "continue" in printS continue
  let debugger = "debugger" in printS debugger
  let delete = "delete" in printS delete
  let enum = "enum" in printS enum
  let export = "export" in printS export
  let extends = "extends" in printS extends
  let finally = "finally" in printS finally
  let for = "for" in printS for
  let function = "function" in printS function
  let implements = "implements" in printS implements
  let instanceof = "instanceof" in printS instanceof
  let interface = "interface" in printS interface
  let new = "new" in printS new
  let null = "null" in printS null
  let package = "package" in printS package
  let private = "private" in printS private
  let protected = "protected" in printS protected
  let public = "public" in printS public
  let return = "return" in printS return
  let static = "static" in printS static
  let super = "super" in printS super
  let switch = "switch" in printS switch
  let this = "this" in printS this
  let throw = "throw" in printS throw
  let try = "try" in printS try
  let typeof = "typeof" in printS typeof
  let undefined = "undefined" in printS undefined
  let var = "var" in printS var
  let void = "void" in printS void
  let while = "while" in printS while
  let with = "with" in printS with
  let yield = "yield" in printS yield

  printS ""
  -- Stdlib functions that need to be encoded
  printS $ const "stdconst" 2
printS :: String -> Fay ()
printS = ffi "console.log(%1)"
