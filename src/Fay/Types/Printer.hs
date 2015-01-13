
module Fay.Types.Printer
  ( PrintReader(..)
  , defaultPrintReader
  , PrintWriter
  , pwOutputString
  , pwMappings
  , Printer(..)
  , Printable(..)
  , execPrinter
  , indented
  , newline
  , write
  , ifPrettyThunks
  , ifPrettyOperators
  , mapping
  ) where

import Control.Monad.RWS
import Data.List                       (elemIndex)
import Data.String
import Language.Haskell.Exts.Annotated 
import SourceMap.Types

-- | Global options of the printer
data PrintReader = PrintReader
  { prPretty          :: Bool      -- ^ Are we to pretty print?
  , prPrettyThunks    :: Bool      -- ^ Use pretty thunk names?
  , prPrettyOperators :: Bool      -- ^ Use pretty operators?
  }

-- | default printer options (non-pretty printing)
defaultPrintReader :: PrintReader
defaultPrintReader = PrintReader False False False

-- | Output of printer
data PrintWriter = PrintWriter
  { pwMappings    :: [Mapping] -- ^ Source mappings.
  , pwOutput      :: ShowS     -- ^ The current output.
  }

pwOutputString :: PrintWriter -> String
pwOutputString (PrintWriter _ out) = out ""

-- | Output concatenation
instance Monoid PrintWriter where
  mempty =  PrintWriter [] id
  mappend (PrintWriter a b) (PrintWriter x y) = PrintWriter (a ++ x) (b . y)

-- | The state of the pretty printer.
data PrintState = PrintState
  { psLine        :: Int       -- ^ The current line.
  , psColumn      :: Int       -- ^ Current column.
  , psIndentLevel :: Int       -- ^ Current indentation level.
  , psNewline     :: Bool      -- ^ Just outputted a newline?
  }

-- | Default state.
defaultPrintState :: PrintState
defaultPrintState = PrintState 0 0 0 False

-- | The printer.
newtype Printer = Printer
  { runPrinter :: RWS PrintReader PrintWriter PrintState () }

execPrinter :: Printer -> PrintReader -> PrintWriter
execPrinter (Printer p) r = snd $ execRWS p r defaultPrintState


instance Monoid Printer where
  mempty = Printer $ return ()
  mappend (Printer p) (Printer q) = Printer (p >> q)

-- | Print some value.
class Printable a where
  printJS :: a -> Printer

-- | Print the given printer indented.
indented :: Printer -> Printer
indented (Printer p) = Printer $ asks prPretty >>= \pretty ->
    when pretty (addToIndentLevel 1) >> p >> when pretty (addToIndentLevel (-1))
  where addToIndentLevel d = modify (\ps -> ps { psIndentLevel = psIndentLevel ps + d })

-- | Output a newline and makes next line indented when prPretty is True.
--   Does nothing whenprPretty is False
newline :: Printer
newline = Printer $ asks prPretty >>= flip when writeNewline
  where writeNewline = (writeRWS "\n" >> modify (\s -> s {psNewline = True}))

-- | Write out a raw string, respecting the indentation
--   Note: if you pass a string with newline characters, it will print them
--   out even if prPretty is set to False. Also next line won't be indented.
--   If you want write a smart newline (that is the one which will be written
--   out only if prPretty is true, and after which the line will be indented)
--   use `newline`)
write :: String -> Printer
write = Printer . writeRWS

writeRWS :: String -> RWS PrintReader PrintWriter PrintState ()
writeRWS x = do
  ps <- get
  let out = if psNewline ps
               then replicate (2 * psIndentLevel ps) ' ' ++ x
               else x
  tell mempty { pwOutput = (out++) }

  let newLines = length (filter (== '\n') x)
  put ps { psLine    = psLine ps + newLines
         , psColumn  = case elemIndex '\n' (reverse x) of
                        Just i  -> i
                        Nothing -> psColumn ps + length x
         , psNewline = False
         }

-- | Write out a string, updating the current position information.
  
instance IsString Printer where
  fromString = write

-- | exec one of Printers depending on PrintReader property.
asksIf :: (PrintReader -> Bool) -> Printer -> Printer -> Printer
asksIf f (Printer p) (Printer q) = Printer $ asks f >>= (\b -> if b then p else q)

-- | Test prPrettyOperator flag
ifPrettyOperators :: Printer -> Printer -> Printer
ifPrettyOperators = asksIf prPrettyOperators

-- | Test prPrettyThunks flag
ifPrettyThunks :: Printer -> Printer -> Printer
ifPrettyThunks = asksIf prPrettyThunks

-- | Generate a mapping from the Haskell location to the current point in the output.
mapping :: SrcSpan -> Printer
mapping srcSpan = Printer $ get >>= \ps -> 
    let m = Mapping { mapGenerated = Pos (fromIntegral (psLine ps))
                                         (fromIntegral (psColumn ps))
                    , mapOriginal = Just (Pos (fromIntegral (srcSpanStartLine srcSpan))
                                              (fromIntegral (srcSpanStartColumn srcSpan) - 1))
                    , mapSourceFile = Just (srcSpanFilename srcSpan)
                    , mapName = Nothing
                    }
    in tell $ mempty { pwMappings = [m] }
