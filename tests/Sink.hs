-- https://github.com/faylang/fay/issues/285
module Main where

import Prelude
import FFI

run :: Fay ()
run = do
    runSink (Sink putStrLn) "hello"

newtype Sink a = Sink { runSink :: a -> Fay () }

main :: Fay ()
main = run
