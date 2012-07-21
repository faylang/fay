{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

-- | Generate documentation for Fay.

module Main where

import           Language.Fay

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.List (isSuffixOf,sort)
import           Prelude hiding (head,div)
import           System.Directory
import           System.FilePath
import           Text.Blaze.Extra
import           Text.Blaze.Html5 as H hiding (contents,map)
import           Text.Blaze.Html5.Attributes as A hiding (title)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)

-- | Main entry point.
main :: IO ()
main = do
  generate >>= L.putStr
  putStrLn ""
  
generate = do
  sources <- mapM readFile examples
  javascripts <- mapM compile examples
  return $ renderMarkup $ page (zip3 (map titlize examples) sources javascripts)
    
  where compile file = do
          contents <- readFile file
          putStrLn $ "Compiling " ++ file ++ " ..."
          result <- compileViaStr compileModule contents
          case result of
            Right javascript -> return javascript
            Left err -> throw err
        titlize = takeWhile (/='.') . upperize . takeFileName
          where upperize (x:xs) = toUpper x : xs
                upperize xs = xs
        examples = map (("docs" </> "snippets") </>)
                       (map (++".hs")
                            ["declarations","conditions"])

page examples = do
  docType
  html $ do
    head $ thehead
    body $ thebody examples

thehead = do
  title $ "The Fay Programming Language — A Haskell subset"
  meta ! httpEquiv "Content-Type" ! content "charset=utf-8"
  link ! rel "stylesheet" ! type_ "text/css" ! href "home.css"
  script ! src "highlight.pack.js" $ return ()
  script ! src "jquery.js" $ return ()
  script ! src "beautify.js" $ return ()
  script ! src "home.js" $ return ()

thebody examples = do
  div !. "wrap" $ do
    theheading
    theexamples examples

theheading = do
  h1 "Fay programming language"
  div !. "subheadline" $ "A strict subset of Haskell that compiles to JavaScript"

theexamples examples = do
  h2 "Examples"
  p $ do "The following examples are generated from the "
         code "examples"
         " directory in the Fay project distribution. "
         "On the left is the Fay code, on the right is the generated JavaScript."
  p $ do "As you can see, there is currently some redundancy in unnecessary thunks and \
         \handling cases that will never arise, but these things are easy to \
         \fix and are not madly expensive."
  forM_ examples $ \(file,fay,javascript) -> do
    h3 $ toHtml file
    div !. "example" $ do
      table $
        tr $ do
          td $ do
            div !. "lang" $ em "Fay"
            pre $
              code !. "language-haskell" $
                toHtml fay
          td $ do
            div !. "lang" $ em "JavaScript"
            pre $
              code !. "language-javascript" $
                toHtml javascript
