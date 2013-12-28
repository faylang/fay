-- https://github.com/faylang/fay/issues/285
module Sink where

import FFI

run :: Fay ()
run = runSink (Sink putStrLn) "hello"

newtype Sink a = Sink { runSink :: a -> Fay () }

main :: Fay ()
main = run
