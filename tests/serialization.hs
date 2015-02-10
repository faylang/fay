module Serialization where

import FFI

data Parametric a = Parametric a
data ConcreteRecord = ConcreteRecord { concreteField :: Double }
main = do
  printParametricButConcreteType (Parametric (ConcreteRecord 123))
  printParametricButAutomaticType (Parametric (ConcreteRecord 123))
  printParametricType (Parametric (ConcreteRecord 123))
  printMaybeConcrete (Just (ConcreteRecord 42))
  printMaybeAutomatic (Just (ConcreteRecord 42))
  printMaybe (Just (ConcreteRecord 42))
  printUnknown 42
  printUnknownField (Just 42)
  printAutomatic (Just (ConcreteRecord 42))

printParametricButConcreteType :: Parametric ConcreteRecord -> Fay ()
printParametricButConcreteType = ffi "console.log(%1)"

printParametricButAutomaticType :: Parametric (Automatic a) -> Fay ()
printParametricButAutomaticType = ffi "console.log(%1)"

printParametricType :: Parametric a -> Fay ()
printParametricType = ffi "console.log(%1)"

printMaybeConcrete :: Maybe ConcreteRecord -> Fay ()
printMaybeConcrete = ffi "console.log(%1)"

printMaybeAutomatic :: Maybe (Automatic a) -> Fay ()
printMaybeAutomatic = ffi "console.log(%1)"

printMaybe :: Maybe a -> Fay ()
printMaybe = ffi "console.log(%1)"

printUnknown :: a -> Fay ()
printUnknown = ffi "console.log(%1)"

printUnknownField :: Maybe a -> Fay ()
printUnknownField = ffi "console.log(%1)"

printAutomatic :: Automatic a -> Fay ()
printAutomatic = ffi "console.log(%1)"
