data Maybe a
  = Just a
  | Nothing

show :: (Foreign a,Show a) => a -> String
show = ffi "JSON.stringify(%1)"

-- There is only Double in JS.
fromInteger x = x
fromRational x = x
