{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Fay.Prelude
  (Fay
  ,Char
  ,String
  ,Integer
  ,Double
  ,Bool(..)
  ,Show(show)
  ,Read
  ,Maybe(..)
  ,read
  ,fromInteger
  ,fromRational
  ,(>>)
  ,(>>=)
  ,(==)
  ,(/=)
  ,(+)
  ,(*)
  ,(-)
  ,(>)
  ,(<)
  ,(||)
  ,(&&)
  ,fail
  ,return
  ,module Language.Fay.Stdlib)
  where

import Language.Fay.Types (Fay)
import Language.Fay.Stdlib

import Prelude ((>),(<),(==),(||),(&&),Maybe(..),Double,Ord,Integer,error,String,(+),Bool(..),Char,Show(..)
               ,Read(..),read,(/=),(*),(-))
import GHC.Real (Ratio)

-- | Just to satisfy GHC.
fromInteger :: Integer -> Double
fromInteger = error "Language.Fay.Prelude.fromInteger: Used fromInteger outside JS."

-- | Just to satisfy GHC.
fromRational :: Ratio Integer -> Double
fromRational = error "Language.Fay.Prelude.fromRational Used fromRational outside JS."

(>>) :: Fay a -> Fay b -> Fay b
(>>) = error "Language.Fay.Prelude.(>>): Used (>>) outside JS."

(>>=) :: Fay a -> (a -> Fay b) -> Fay b
(>>=) = error "Language.Fay.Prelude.(>>=): Used (>>=) outside JS."

fail :: String -> Fay a
fail = error "Language.Fay.Prelude.fail: Used fail outside JS."

return :: a -> Fay a
return = error "Language.Fay.Prelude.return: Used return outside JS."
