module WhenUnlessRecursion where

import FFI

getStackSize :: Fay Int
getStackSize = ffi "(Error.stackTraceLimit = Infinity, new Error().stack.split('\\n').length)"

checkGrowth :: Maybe Int -> Int -> Fay ()
checkGrowth Nothing _      = return ()
checkGrowth (Just old) new = if new == old then return () else (error $ "Call stack growth: " ++ show old ++ " to " ++ show new)

main = do
  putStrLn "loopIf"     >> loopIf     Nothing 0
  putStrLn "loopWhen"   >> loopWhen   Nothing 0
  putStrLn "loopUnless" >> loopUnless Nothing 0
  where
    pred = (< 5)
    step = (+ 1)
    action f s n = getStackSize >>= \s' -> checkGrowth s s' >> f (Just s') (step n)
    loopIf     s n = if          (pred n) then (action loopIf     s n) else return ()
    loopWhen   s n = when        (pred n)      (action loopWhen   s n)
    loopUnless s n = unless (not (pred n))     (action loopUnless s n)
