-- | Simple local storage bindings.

module Data.LocalStorage where

import Data.Text
import FFI
import Prelude

setLocalStorage :: Text -> Text -> Fay ()
setLocalStorage = ffi "(function() { localStorage[%1] = %2; })()"

getLocalStorage :: Text -> Fay (Defined Text)
getLocalStorage = ffi "localStorage[%1]"

removeLocalStorage :: Text -> Fay ()
removeLocalStorage = ffi "localStorage.removeItem(%1)"

hasLocalStorage :: Bool
hasLocalStorage = ffi "typeof(Storage) !== 'undefined'"
