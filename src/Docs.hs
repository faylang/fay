{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

-- | Generate documentation for Fay.

module Main where

import           Language.Fay                (compileForDocs, compileViaStr)
import           Language.Fay.Compiler       (compileFromTo)
import           Language.Fay.Types          (CompileConfig (..))

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy        as L
import           Data.Char
import           Data.Default

import           Data.Time
import           Prelude                     hiding (div, head)

import           System.FilePath
import           Text.Blaze.Extra
import           Text.Blaze.Html5            as H hiding (contents, map, style)
import           Text.Blaze.Html5.Attributes as A hiding (title)
import           Text.Blaze.Renderer.Utf8    (renderMarkup)

-- | Main entry point.
main :: IO ()
main = do
  let file = "docs" </> "index.html"
  generate >>= L.writeFile file
  generateJs
  putStrLn $ "Documentation file written to " ++ file

generate = do
  sources <- mapM readFile examples
  javascripts <- mapM compile examples
  now <- getCurrentTime
  analytics <- readFile $ "docs" </> "analytics.js"
  return $ renderMarkup $ page now
                               analytics
                               (zip3 (map titlize examples) sources javascripts)

  where compile file = do
          contents <- readFile file
          putStrLn $ "Generating " ++ file ++ " ..."
          result <- compileViaStr def { configFlattenApps = True, configTCO = True } compileForDocs contents
          case result of
            Right (javascript,_) -> return javascript
            Left err -> throw err
        titlize = takeWhile (/='.') . upperize . takeFileName
          where upperize (x:xs) = toUpper x : xs
                upperize xs = xs
        examples = map (("docs" </> "snippets") </>)
                       (map (++".hs")
                            (words "declarations conditions functions lists data enums patterns ffi dom tail"))

generateJs = do
  putStrLn $ "Compiling " ++ inp ++ " to " ++ out ++ " ..."
  compileFromTo def { configAutorun = True, configFlattenApps = True } inp out

  where docs = ("docs" </>)
        inp = docs "home.hs"
        out = docs "home.js"

page now analytics examples = do
  docType
  html $ do
    head $ thehead
    body $ thebody now analytics examples

thehead = do
  title $ "The Fay Programming Language — A Haskell subset"
  meta ! httpEquiv "Content-Type" ! content "charset=utf-8"
  link ! rel "stylesheet" ! type_ "text/css" ! href "home.css"
  script ! src "highlight.pack.js" $ return ()
  script ! src "jquery.js" $ return ()
  script ! src "beautify.js" $ return ()
  script ! src "home.js" $ return ()

thebody now analytics examples = do
  fork
  div !. "wrap" $ do
    theheading
    theintro
    thesetup
    thejsproblem
    thecomparisons
    theexamples examples
    thereference
    thefooter now
  preEscapedToMarkup analytics

fork =
  a ! href "https://github.com/faylang/fay" $
    img ! style "position: absolute; top: 0; right: 0; border: 0;"
        ! src "https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"
        ! alt "Fork me on GitHub"

theheading = do
  h1 "Fay programming language"
  div !. "subheadline" $ "A proper subset of Haskell that compiles to JavaScript"

theexamples examples = do
  a ! name "examples" $ return ()
  h2 "Examples"
  p $ do "The following examples are generated from the "
         code "docs/snippets"
         " directory in the Fay project distribution. "
         "The Fay tab of each example shows the original Fay code, and the "
         " JavaScript tab shows the actual generated output of the compiler "
         " use to generate this document."
  p $ do "Because Fay is lazy, it creates thunks and forces them on a regular basis. "
         "Thus, the symbol "; code "$"; " means “thunk”, and the symbol "; code "_"
         " means “force”."
  forM_ examples $ \(file,fay,javascript) -> do
    h3 !. "example-heading" $ toHtml file
    div !. "example" $ do
      table $ do
        tr $ do
          td $ do
            div !. "panel" $ do
              div !. "lang" $ em "Fay"
              pre !. "pre" $
                code !. "language-haskell" $
                  toHtml fay
          td $ do
            div !. "panel" $ do
              div !. "lang" $ em "JavaScript"
              pre !. "pre" $
                code !. "language-javascript" $
                  toHtml javascript
    div !. "example-clear" $ return ()

thefooter now =
  div !. "footer" $ do
    "© 2012 Chris Done. "
    "Document generated on "
    toHtml (show now)
    " with sexy Haskell. See "
    code $ toHtml (takeFileName __FILE__)
    " for the source of this document generator."

theintro = do
  h2 "Introduction"
  p "Fay is a small programming language which has the following properties:"
  ul $ do
    li $ do "A proper syntactic and semantic subset of "
            a ! href "http://haskell.org/" $ "Haskell"
    li $ "Statically typed"
    li $ "Lazy"
    li $ "Pure by default"
    li $ "Compiles to JavaScript"
    li $ "Has fundamental data types (Double, String, etc.) based upon what JS can support"
    li $ "Outputs minifier-aware code for small compressed size"
    li $ "Has a trivial foreign function interface to JavaScript"
  p $ do "Because Fay is intended to be small and simple, it relies on GHC, the Haskell compiler, "
         "for static checking. So the workflow when working with Fay is: "
  pre $ do
    "$ ghc hello.hs\n"
    "$ fay hello.hs"
  p $ do "The "; code "ghc"; " call will compile the Fay (really, Haskell) code and run the type system, "
         "and the call to "; code "fay"
         " will actually generate JavaScript code under "; code "hello.js"; "."
  p $ a ! href "#examples" $ "Go to examples »"

thejsproblem = do
  h2 "The JavaScript Problem"
  p "The JavaScript problem is two-fold and can be described thus:"
  ul $ do
    li $ do strong "JavaScript sucks:"
            " The depths to which JavaScript sucks is well-documented and well-understood. Its main faults are: its lack of module system, weak-typing, verbose function syntax, late binding, which has led to the creation of various static analysis tools to alleviate this language flaw, but with limited success (there is even a static type checker), finicky equality/automatic conversion, this behaviour, and lack of static types."
    li $ do strong "We need JavaScript:"
            " Using it for what it is good for, i.e. providing a platform for browser development, but not using the language per se, is therefore desirable, and many are working to achieve this, in varying forms. There are various ways to do it, but we ought to opt for compiling an existing language, Haskell, to JavaScript, because we do not have time to learn or teach other people a new language, garner a new library set and a new type checker and all that Haskell implementations provide."
  p $ a ! href "http://www.haskell.org/haskellwiki/The_JavaScript_Problem" $
        "See here for more elaboration »"

thecomparisons = do
  h2 "Comparisons to other methods"
  coffee
  royandelm

coffee = do
  h3 "CoffeeScript and LiveScript"
  p $ do "CoffeeScript is a syntactic layer above JavaScript that does not change semantics. "
         "It adds some additional syntactic constructs, but makes no fundamental changes, "
         "you are still essentially working in JavaScript, but with more convenient "
         "syntactic features."
  p $ do "Fay on the other hand is a different language to JavaScript entirely, with a "
         "different semantics. It is lazy, it has partial application and currying, "
         "pattern matching for all data types, all expressions are pure and only "
         "statements in the Fay monad can be impure."
  p $ do "LiveScript is also a similar approach in the wave of compile-to-JS projects "
         "that have developed in recent years. LiveScript's translation is also quite "
         "readable and predictable, this is also the only thing in common with Fay."

royandelm = do
  h3 "Roy and Elm"
  p $ do "Roy is an approach to bring functional programming to JavaScript, it has lots of "
         "interesting features but it has different syntax and type-system semantics "
         "to Haskell."
  p $ do "Elm, equally, is an approach to bringing functional programming to the web, "
         "and is less generic than Roy, as it specifically focuses on web programming. It, too, "
         "borrows from Haskell and looks a bit like it, but is (consciously) different in "
         "syntax and semantics."
  p $ do "Both of these languages are very interesting and promising approaches. What Fay offers "
         "is to keep the existing compiler, GHC, for its battle-tested type checking and "
         " code analysis, and to use existing parsers for Haskell to support a subset of its "
         "syntax. This way, one does not have to replace the tooling infrastructure and workflow "
         "that one is used to. With the exception of actions in the Fay monad, pure functions "
         "can be type checked and ran within GHCi."
  p $ do "Additionally, because all Fay code "; em "is"; " Haskell code, certain modules "
         "can be shared between the ‘native’ Haskell and ‘web’ Haskell, most interestingly "
         "the types module of your project. This enables two things:"
  ul $ do
    li "The enforced (by GHC) coherence of client-side and server-side data types."
    li $ do "The transparent serializing and deserializing of data types between these "
            "two entities (i.e. over AJAX)."

thesetup = do
  h2 "Installation"
  p $ do "The Fay compiler is written in Haskell, so you will need "
         a ! href "http://hackage.haskell.org/platform/" $ "the Haskell platform"
         " installed (or at least Cabal)."
  h3 "Cabal install from Hackage"
  p "To install, run:"
  pre $ code "$ cabal install fay"
  h3 "From Github"
  p "If you want to hack on the compiler, you can download the Git repo: "
  pre $ code "$ git clone git://github.com/faylang/fay.git"
  p "And then install from the directory: "
  pre $ code "$ cabal install"
  h3 "Running"
  p "If developing, you can run the tests (you will need nodejs installed):"
  pre $ code "$ cabal install -fdevel"
  pre $ code "$ fay-tests"
  p "To compile a Fay program, run:"
  pre $ code "$ fay --autorun foo.hs"
  p $ do "The "; code "--autorun"; " flag will make sure that the "; code "main"; " function is called."
  p "You can also install this via cabal-dev, but be sure to run the commands from the cabal-dev bin dir: "
  pre $ code "$ cabal-dev install\n$ cabal-dev/bin/fay --autorun foo.hs"
  h3 "Generate this Documentation"
  p "This web page is generated by the following call in the project directory:"
  pre $ code "$ fay-docs"
  p $ do "If you are hacking on the project this can be useful to inspect, and "
         "if you add more features, please document them in "; code $ toHtml (takeFileName __FILE__); "."

thereference = do
  h2 "Language Reference"
  p $ do "To be written. There is a great deal of syntax not yet supported, which I will document. "
         "For now it is best to simply try and see if you get an “Unsupported X” compile error or not. "
         "It will not accept things that it doesn't support, apart from class and instance declarations, "
         "which it ignores entirely. Inspect the compiler source if you are unsure, it is rather simple."
