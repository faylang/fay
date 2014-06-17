import FFI

main = do putStrLn "Hey ho!"
          setTimeout 500 doThing
          doThing

doThing = putStrLn "Hello, World!"

setTimeout :: Int -> (Fay ()) -> Fay ()
setTimeout = ffi "(function (f,i) { var id = setTimeout(function () { f(id); }, i); return id; })(%2,%1)"
