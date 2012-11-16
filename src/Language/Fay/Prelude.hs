{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Fay.Prelude
  (Fay
  ,Char
  ,String
  ,Integer
  ,Double
  ,Int
  ,Bool(..)
  ,Show
  ,Read
  ,Maybe(..)
  ,Typeable(..)
  ,Data(..)
  ,Monad
  ,Eq(..)
  ,read
  ,(>>)
  ,(>>=)
  ,(+)
  ,(*)
  ,(-)
  ,(>)
  ,(<)
  ,(>=)
  ,(<=)
  ,(/)
  ,(||)
  ,(&&)
  ,fail
  ,return
  ,module Language.Fay.Stdlib)
  where

import           Language.Fay.Stdlib
import           Language.Fay.Types  (Fay)
import           Data.Data
import           Prelude             (Bool(..), Char, Double, Eq(..), Int, Integer, Maybe(..), Monad,
 Ord, Read(..), Show(), String, error, read, (&&), (*), (+), (-),
 (/), (/=), (<), (<=), (==), (>), (>=), (||))

(>>) :: Fay a -> Fay b -> Fay b
(>>) = error "Language.Fay.Prelude.(>>): Used (>>) outside JS."
infixl 1 >>

(>>=) :: Fay a -> (a -> Fay b) -> Fay b
(>>=) = error "Language.Fay.Prelude.(>>=): Used (>>=) outside JS."
infixl 1 >>=

fail :: String -> Fay a
fail = error "Language.Fay.Prelude.fail: Used fail outside JS."

return :: a -> Fay a
return = error "Language.Fay.Prelude.return: Used return outside JS."
