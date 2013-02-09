data Parametric a = Parametric a
data ConcreteRecord = ConcreteRecord { concreteField :: Double }
main = do
  printParametricButConcreteType (Parametric (ConcreteRecord 123))

printParametricButConcreteType :: Parametric ConcreteRecord -> Fay ()
printParametricButConcreteType = ffi "console.log(%1)"
