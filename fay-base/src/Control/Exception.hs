-- | Exception handling.

module Control.Exception where

import FFI

-- | Try the given action and catch if there's an error.
onException :: Fay a -> Fay a -> Fay a
onException = ffi "function() { try { return %1(); } catch(e) { return %2(); } }()"

-- | Try the given action and catch the exception.
catch :: Fay a -> (Automatic e -> Fay a) -> Fay a
catch = ffi "function() { try { return %1(); } catch(e) { return %2(e); } }()"

-- | Throw an exception.
throw :: Automatic e -> Fay a
throw = ffi "(function() { throw %1 })()"
