{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module ModuleReExport.ExportsModule
  ( module ModuleReExport.ExportsIdentifier
  , module Prelude
  , foo
  )
  where

import ModuleReExport.ExportsIdentifier
import Prelude hiding (div)

foo = div
