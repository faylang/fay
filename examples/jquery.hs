{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module JQuery where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  ready $ do
    print (showDouble 123)
    body <- select "body"
    printArg body
    addClassWith (\i s -> do print ("i… " ++ showDouble i)
                             print ("s… " ++ showString s)
                             return "abc")
                 body
    addClassWith (\i s -> do print ("i… " ++ showDouble i)
                             print ("s… " ++ showString s)
                             print (showString ("def: " ++ s))
                             return "foo")
                 body
    printArg body
    return ()

data JQuery
instance Foreign JQuery
instance Show JQuery

data Element
instance Foreign Element

print :: String -> Fay ()
print = ffi "console.log(%1)"

printArg :: Foreign a => a -> Fay ()
printArg = ffi "console.log(\"%%o\",%1)"

showDouble :: Double -> String
showDouble = ffi "(%1).toString()"

showString :: String -> String
showString = ffi "JSON.stringify(%1)"

select :: String -> Fay JQuery
select = ffi "jQuery(%1)"

addClassWith :: (Double -> String -> Fay String) -> JQuery -> Fay JQuery
addClassWith = ffi "%2.addClass(%1)"

ready :: Fay () -> Fay ()
ready = ffi "jQuery(%1)"
