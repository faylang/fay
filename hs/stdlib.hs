data ArgType
  = DateType
  | FunctionType
  | JsType
  | StringType
  | DoubleType
  | ListType
  | BoolType
  | UnknownType

data Maybe a
  = Just a
  | Nothing

show :: (Foreign a,Show a) => a -> String
show = ffi "Fay$$encodeShow(%1)" FayString

-- There is only Double in JS.
fromInteger x = x
fromRational x = x