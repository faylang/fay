{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Home (main) where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  ready (jquery thedocument) $ do
    samples <- query ".language-javascript"
    forM_ samples $ \this ->
      let sample = jquery this
      in do text <- getText sample
            setText sample
                    (beautify text 2)
    setTabReplace hljs "    "
    initHighlightingOnLoad hljs

--------------------------------------------------------------------------------
-- Window object

-- | Console log.
print :: (Show a,Foreign a) => a -> Fay ()
print = foreignFay "console.log" FayNone

-- | Pop-up an alert.
alert :: String -> Fay ()
alert = foreignFay "window.alert" FayNone

--------------------------------------------------------------------------------
-- DOM

data Element
instance Foreign Element
instance Show Element

-- | The document.
thedocument :: Element
thedocument = foreignValue "window.document" FayNone

--------------------------------------------------------------------------------
-- jQuery

data JQuery
instance Foreign JQuery

-- | Make a jQuery object out of an element.
jquery :: Element -> JQuery
jquery = foreignPure "window.jQuery" FayNone

-- | Bind a handler for when the element is ready.
ready :: JQuery -> Fay () -> Fay ()
ready = foreignMethodFay "ready" FayNone

-- | Query for elements.
query :: String -> Fay [Element]
query = foreignFay "window.jQuery" FayList

-- | Set the text of the given object.
setText :: JQuery -> String -> Fay ()
setText = foreignMethodFay "text" FayNone

-- | Get the text of the given object.
getText :: JQuery -> Fay String
getText = foreignMethodFay "text" FayString

--------------------------------------------------------------------------------
-- Pretty printing / highlighting

-- | Pretty print the given JS code.
beautify :: String -- ^ The JS code.
         -> Double -- ^ The indentation level.
         -> String -- ^ The reformatted JS code.
beautify = foreignPure "$jsBeautify" FayString

data Highlighter
instance Foreign Highlighter

-- | Get the highlighter.
hljs :: Highlighter
hljs = foreignValue "window.hljs" FayNone

-- | Init syntax highlighting on load.
initHighlightingOnLoad :: Highlighter -> Fay ()
initHighlightingOnLoad = foreignMethodFay "initHighlightingOnLoad" FayNone

-- | Init syntax highlighting on load.
setTabReplace :: Highlighter -> String -> Fay ()
setTabReplace = foreignSetProp "tabReplace"
