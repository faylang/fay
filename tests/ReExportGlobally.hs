module ReExportGlobally (main, x) where

import           FFI
import           Prelude

import           ReExportGlobally.A (x)

main :: Fay ()
main = do
  ffi "console.log(ReExportGlobally.x)" :: Fay () -- Re-export to JS
  ffi "console.log('NewTy' in ReExportGlobally.A)" :: Fay () -- Don't add exports for new types
  ffi "console.log('NewTy' in ReExportGlobally)" :: Fay () -- Don't add exports for new types
  ffi "console.log('unNewTy' in ReExportGlobally.A)" :: Fay () -- Don't add exports for new types
  ffi "console.log('unNewTy' in ReExportGlobally)" :: Fay () -- Don't add exports for new types
