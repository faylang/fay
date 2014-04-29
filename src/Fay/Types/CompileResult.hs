module Fay.Types.CompileResult (CompileResult (..)) where

import           SourceMap.Types

data CompileResult = CompileResult
  { resOutput         :: String
  , resImported       :: [(String, FilePath)]
  , resSourceMappings :: Maybe [Mapping]
  } deriving Show
