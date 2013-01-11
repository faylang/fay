-- $ fay --print-runtime > examples/separated-rts.js
-- $ fay --include examples --stdlib --no-dispatcher --naked --no-ghc --no-rts --pretty examples/Separated.hs -o examples/separated-stdlib.js
-- $ fay --include examples --no-stdlib --dispatcher --naked --no-ghc --no-rts --pretty examples/Separated.hs -o examples/separated-dispatcher.js
-- $ fay --include examples --no-stdlib --no-dispatcher --no-ghc --no-rts --naked --pretty examples/Y.hs -o examples/separated-y.js
-- $ fay --include examples --no-stdlib --no-dispatcher --no-ghc --no-rts --naked --pretty examples/X.hs -o examples/separated-x.js
-- $ (cat examples/separated-rts.js; cat examples/separated-stdlib.js; cat examples/separated-dispatcher.js; cat examples/separated-y.js; echo 'Fay$$_(Y$main)') | node
-- { instance: 'Bar', slot1: [ 1, 2, 3 ], slot2: 'Hello, Bar!' }
-- $ (cat examples/separated-rts.js; cat examples/separated-stdlib.js; cat examples/separated-dispatcher.js; cat examples/separated-x.js; echo 'Fay$$_(X$main)') | node
-- { instance: 'Foo', slot1: [ 1, 2, 3 ], slot2: 'Hello, Foo!' }
-- $

module Separated where

import FFI
import Prelude

import X
import Y
