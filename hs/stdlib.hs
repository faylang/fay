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
