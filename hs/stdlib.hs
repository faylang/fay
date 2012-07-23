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
show = foreignPure "Fay$$encodeShow" "string"
