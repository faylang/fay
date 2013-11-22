-- This module needs to be top level to do the intended test.
module ModuleRecordClash2_Hello where

import           Prelude

defaultHello :: ModuleRecordClash2_Hello
defaultHello = ModuleRecordClash2_Hello { greeting = "Hello, world!" }

data ModuleRecordClash2_Hello = ModuleRecordClash2_Hello { greeting :: String }
