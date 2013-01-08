{- NOTE: This file is also used in the Api tests. -}

module RecordImport_Export where

import           Prelude

data R = R Integer
data Fields = Fields { fieldFoo :: Integer, fieldBar :: Integer }
