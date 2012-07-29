{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page script:
-- 
--  1) Make sure that the language samples are highlighted.
--  2) Add a show/hide button for the JS output on all samples.
--  3) For code samples that are too large, hide them.
-- 
-- Because dogfood. This is not necessarily a good example, indeed, it
-- may very well be a very bad example of how to write client
-- code. But that's the point, really. Even the quick scripts that
-- you'd just whip up in JS should be also done in Fay. Alright?
--

module Home (main) where

import Language.Fay.FFI
import Language.Fay.Prelude

-- | Main entry point.
main :: Fay ()
main = do
  ready (wrap [thedocument]) $ do
    indentAndHighlight
    setupExpanders
    setupTableOfContents

-- | Setup highlighting/code re-indenting.
indentAndHighlight :: Fay ()
indentAndHighlight = do
  samples <- query ".language-javascript"
  each samples $ \i this ->
    let sample = wrap [this]
    in do text <- getText sample
          setText sample
                  (beautify text 2)
  setTabReplace hljs "    "
  initHighlightingOnLoad hljs

-- | Setup example expanders.
setupExpanders :: Fay ()
setupExpanders = do
  wrapwidth <- query ".wrap" >>= getWidth
  examples <- query ".example"
  each examples $ \i this -> do
    tr <- getFind (wrap [this]) "tr"
    left <- getFind tr "td" >>= getFirst
    addClass left "left"
    right <- getNext left
    toggle <- makeElement "<a class='toggle' href='javascript:'>Show JavaScript</a>"
    toggleButton <- return $ do
      visible <- getIs right ":visible"
      if visible
         then do setText toggle "Hide JavaScript"
                 swapClasses toggle "toggle-hide" "toggle-show"
         else do setText toggle "Show JavaScript"
                 swapClasses toggle "toggle-show" "toggle-hide"
    setClick toggle $ fadeToggle right $ toggleButton
    width <- getWidth tr
    when (width > wrapwidth + 20*wrapwidth/100) $ hide right
    toggleButton
    panel <- getFind left ".panel"
    prepend panel toggle
    return ()

-- | Generate a table of contents.
setupTableOfContents :: Fay ()
setupTableOfContents = do
  toc <- makeElement "<div class='table-of-contents'><p>Table of Contents</p></div>"
  query ".subheadline" >>= flip after toc
  ul <- makeElement "<ul></ul>" >>= flip appendTo toc
  headings <- query "h2"
  each headings $ \i heading ->
    let anchor = ("section-" ++ show i)
        h = wrap [heading]
    in do -- Make sure the anchor exists at the heading point.
          attr h "id" anchor
          -- Make the entry.
          li <- makeElement "<li></li>" >>= flip appendTo ul
          a <- makeElement "<a></a>" >>= flip appendTo li
          getText h >>= setText a
          -- Link up to an anchor.
          attr a "href" ("#" ++ anchor)
          -- For the indentation.
          getTagName heading >>= addClass li

--------------------------------------------------------------------------------
-- Window object

-- | Print something.
print :: Show a => a -> Fay ()
print = consolelog . show

-- | Console log.
consolelog :: String -> Fay ()
consolelog = ffi "console.log(%1)" FayNone

-- | Pop-up an alert.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)" FayNone

--------------------------------------------------------------------------------
-- DOM

data Element
instance Foreign Element
instance Show Element

-- | The document.
thedocument :: Element
thedocument = ffi "window.document" FayNone

-- | Get the size of the given jquery array.
getTagName :: Element -> Fay String
getTagName = ffi "%1['tagName']" FayString

--------------------------------------------------------------------------------
-- jQuery

data JQuery
instance Foreign JQuery
instance Show JQuery

-- | Make a jQuery object out of an element.
wrap :: [Element] -> JQuery
wrap = ffi "window['jQuery'](%1)" FayNone

-- | Bind a handler for when the element is ready.
ready :: JQuery -> Fay () -> Fay ()
ready = ffi "%1['ready'](%2)" FayNone

-- | Bind a handler for when the element is ready.
each :: JQuery -> (Double -> Element -> Fay ()) -> Fay ()
each = ffi "%1['each'](%2)" FayNone

-- | Query for elements.
query :: String -> Fay JQuery
query = ffi "window['jQuery'](%1)" FayNone

-- | Set the text of the given object.
setText :: JQuery -> String -> Fay ()
setText = ffi "%1['text'](%2)" FayNone

-- | Set the text of the given object.
attr :: JQuery -> String -> String -> Fay ()
attr = ffi "%1['attr'](%2,%3)" FayNone

-- | Set the click of the given object.
setClick :: JQuery -> Fay () -> Fay ()
setClick = ffi "%1['click'](%2)" FayNone

-- | Toggle the visibility of an element, faded.
fadeToggle :: JQuery -> Fay () -> Fay ()
fadeToggle = ffi "%1['fadeToggle'](%2)" FayNone

-- | Hide an element.
hide :: JQuery -> Fay ()
hide = ffi "%1['hide']()" FayNone

-- | Add a class to the given object.
addClass :: JQuery -> String -> Fay ()
addClass = ffi "%1['addClass'](%2)" FayNone

-- | Remove a class from the given object.
removeClass :: JQuery -> String -> Fay ()
removeClass = ffi "%1['removeClass'](%2)" FayNone

-- | Swap the given classes on the object.
swapClasses :: JQuery -> String -> String -> Fay ()
swapClasses obj addme removeme = do
  addClass obj addme
  removeClass obj removeme

-- | Get the text of the given object.
getText :: JQuery -> Fay String
getText = ffi "%1['text']()" FayString

-- | Get the text of the given object.
getIs :: JQuery -> String -> Fay Bool
getIs = ffi "%1['is'](%2)" FayBool

-- | Get the size of the given jquery array.
getSize :: JQuery -> Fay Double
getSize = ffi "%1['length']" FayNone

-- | Get the next of the given object.
getNext :: JQuery -> Fay JQuery
getNext = ffi "%1['next']()" FayNone

-- | Get the first of the given object.
getFirst :: JQuery -> Fay JQuery
getFirst = ffi "%1['first']()" FayNone

-- | Get the find of the given object.
getFind :: JQuery -> String -> Fay JQuery
getFind = ffi "%1['find'](%2)" FayNone

-- | Prepend an element to this one.
prepend :: JQuery -> JQuery -> Fay JQuery
prepend = ffi "%1['prepend'](%2)" FayNone

-- | Append an element /after/ this one.
after :: JQuery -> JQuery -> Fay JQuery
after = ffi "%1['after'](%2)" FayNone

-- | Append an element to this one.
append :: JQuery -> JQuery -> Fay JQuery
append = ffi "%1['append'](%2)" FayNone

-- | Append this to an element.
appendTo :: JQuery -> JQuery -> Fay JQuery
appendTo = ffi "%1['appendTo'](%2)" FayNone

-- | Make a new element.
makeElement :: String -> Fay JQuery
makeElement = ffi "window['jQuery'](%1)" FayNone

-- | Get the width of the given object.
getWidth :: JQuery -> Fay Double
getWidth = ffi "%1['width']()" FayNone

--------------------------------------------------------------------------------
-- Pretty printing / highlighting

-- | Pretty print the given JS code.
beautify :: String -- ^ The JS code.
         -> Double -- ^ The indentation level.
         -> String -- ^ The reformatted JS code.
beautify = ffi "$jsBeautify(%1,%2)" FayString

data Highlighter
instance Foreign Highlighter

-- | Get the highlighter.
hljs :: Highlighter
hljs = ffi "window['hljs']" FayNone

-- | Init syntax highlighting on load.
initHighlightingOnLoad :: Highlighter -> Fay ()
initHighlightingOnLoad = ffi "%1['initHighlightingOnLoad']()" FayNone

-- | Init syntax highlighting on load.
setTabReplace :: Highlighter -> String -> Fay ()
setTabReplace = ffi "%1['tabReplace']=%2" FayNone
