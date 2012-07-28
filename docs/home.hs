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
  ul <- makeElement "<ul></ul>"
  append toc ul
  body <- query "body"
  subheadline <- query ".subheadline"
  after subheadline toc
  headings <- query "h2,h3,h4,h5"
  return ()
  each headings $ \i heading ->
    let anchor = ("section-" ++ show i)
    in do text <- getText (wrap [heading])
          -- Make sure the anchor exists at the heading point.
          attr (wrap [heading]) "id" anchor
          -- 
          li <- makeElement "<li></li>"
          append ul li
          -- Link up to an anchor.
          a <- makeElement "<a></a>"
          attr a "href" ("#" ++ anchor)
          setText a text
          append li a
          -- For the indentation:
          name <- getTagName heading
          addClass li name

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

-- | Get the size of the given jquery array.
getTagName :: Element -> Fay String
getTagName = foreignPropFay "tagName" FayString
  
--------------------------------------------------------------------------------
-- jQuery

data JQuery
instance Foreign JQuery
instance Show JQuery

-- | Make a jQuery object out of an element.
wrap :: [Element] -> JQuery
wrap = foreignPure "window['jQuery']" FayNone

-- | Bind a handler for when the element is ready.
ready :: JQuery -> Fay () -> Fay ()
ready = foreignMethodFay "ready" FayNone

-- | Bind a handler for when the element is ready.
each :: JQuery -> (Double -> Element -> Fay ()) -> Fay ()
each = foreignMethodFay "each" FayNone

-- | Query for elements.
query :: String -> Fay JQuery
query = foreignFay "window['jQuery']" FayNone

-- | Set the text of the given object.
setText :: JQuery -> String -> Fay ()
setText = foreignMethodFay "text" FayNone

-- | Set the text of the given object.
attr :: JQuery -> String -> String -> Fay ()
attr = foreignMethodFay "attr" FayNone

-- | Set the click of the given object.
setClick :: JQuery -> Fay () -> Fay ()
setClick = foreignMethodFay "click" FayNone

-- | Toggle the visibility of an element, faded.
fadeToggle :: JQuery -> Fay () -> Fay ()
fadeToggle = foreignMethodFay "fadeToggle" FayNone

-- | Hide an element.
hide :: JQuery -> Fay ()
hide = foreignMethodFay "hide" FayNone

-- | Add a class to the given object.
addClass :: JQuery -> String -> Fay ()
addClass = foreignMethodFay "addClass" FayNone

-- | Remove a class from the given object.
removeClass :: JQuery -> String -> Fay ()
removeClass = foreignMethodFay "removeClass" FayNone

-- | Swap the given classes on the object.
swapClasses :: JQuery -> String -> String -> Fay ()
swapClasses obj addme removeme = do
  addClass obj addme
  removeClass obj removeme

-- | Get the text of the given object.
getText :: JQuery -> Fay String
getText = foreignMethodFay "text" FayString

-- | Get the text of the given object.
getIs :: JQuery -> String -> Fay Bool
getIs = foreignMethodFay "is" FayBool

-- | Get the size of the given jquery array.
getSize :: JQuery -> Fay Double
getSize = foreignPropFay "length" FayNone

-- | Get the next of the given object.
getNext :: JQuery -> Fay JQuery
getNext = foreignMethodFay "next" FayNone

-- | Get the first of the given object.
getFirst :: JQuery -> Fay JQuery
getFirst = foreignMethodFay "first" FayNone

-- | Get the find of the given object.
getFind :: JQuery -> String -> Fay JQuery
getFind = foreignMethodFay "find" FayNone

-- | Prepend an element to this one.
prepend :: JQuery -> JQuery -> Fay JQuery
prepend = foreignMethodFay "prepend" FayNone

-- | Append an element /after/ this one.
after :: JQuery -> JQuery -> Fay JQuery
after = foreignMethodFay "after" FayNone

-- | Append an element to this one.
append :: JQuery -> JQuery -> Fay JQuery
append = foreignMethodFay "append" FayNone

-- | Make a new element.
makeElement :: String -> Fay JQuery
makeElement = foreignFay "window['jQuery']" FayNone

-- | Get the width of the given object.
getWidth :: JQuery -> Fay Double
getWidth = foreignMethodFay "width" FayNone

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
hljs = foreignValue "window['hljs']" FayNone

-- | Init syntax highlighting on load.
initHighlightingOnLoad :: Highlighter -> Fay ()
initHighlightingOnLoad = foreignMethodFay "initHighlightingOnLoad" FayNone

-- | Init syntax highlighting on load.
setTabReplace :: Highlighter -> String -> Fay ()
setTabReplace = foreignSetProp "tabReplace"
