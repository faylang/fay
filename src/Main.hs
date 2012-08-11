
-- | Main compiler executable.

module Main where

import           Language.Fay.Compiler
import           Language.Fay.Types

import           Control.Arrow
import           Control.Monad
import           Data.Default
import           Data.List
import           Data.Maybe
import           System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  let files = filter (not . isPrefixOf "-") args
      paramOpts = map ((drop 2 *** drop 1) . break (== '=')) $ filter (isPrefixOf "--") args
      opts = map (drop 1) $ filter (\v -> isPrefixOf "-" v && not ("--" `isPrefixOf` v)) args
  if "help" `elem` opts || null files
    then putStrLn helpText
    else forM_ files $ \file ->
      compileFromTo def { configTCO = elem "tco" opts
                        , configInlineForce = elem "inline-force" opts
                        , configFlattenApps = elem "flatten-apps" opts
                        , configExportBuiltins = not (elem "no-export-builtins" opts)
                        , configDirectoryIncludes = maybe [] (split ',') (lookup "include" paramOpts)
                        }
        ("autorun" `elem` opts)
        ("html-wrapper" `elem` opts)
        file
        (fromMaybe (toJsName file) $ lookup "output" paramOpts)
  where
      -- | "12,34,5" => ["12","34","5"]
      split :: Eq a => a -> [a] -> [[a]]
      split _ [] = []
      split a as = takeWhile (/= a) as : split a (drop 1 $ dropWhile (/= a) as)

helpText = unlines
  ["fay -- compiler from (a proper subset of) Haskell to JavaScript"
  ,""
  ,"USAGE"
  ,"  fay [OPTIONS] <hs-input-file> ... "
  ,""
  ,"OPTIONS"
  ,"  -autorun       automatically call main in generated JavaScript"
  ,"  -inline-force  inline forcing, adds some speed for numbers, blows up code a bit"
  ,"  -flatten-apps  flatten function application, can be more readable,"
  ,"                 no noticeable speed difference"
  ,"  -html-wrapper  creates a html file in the same directory as the output that loads"
  ,"                 the compiled file"
  ,"  --include=dir1[, ..]"
  ,"                 looks in these additional directories for imports"
  ,"  --output=file  places the resulting file in this location"
  ]
