import FFI

data Array a

empty :: Fay (Array a)
empty = ffi "[]"

push:: a -> Array a -> Fay ()
push = ffi "%2.push(%1)"

len :: Array a -> Fay Int
len = ffi "%1['length']"

main :: Fay ()
main = do
  arr <- empty
  print arr
  push (5::Int) arr
  print =<< len arr
  print arr
