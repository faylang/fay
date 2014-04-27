module Fay.Compiler.Parse
  ( parseFay
  , defaultExtensions
  ) where

import           Language.Haskell.Exts.Annotated hiding (name)

-- | Parse some Fay code.
parseFay :: Parseable ast => FilePath -> String -> ParseResult ast
parseFay filepath = parseWithMode parseMode { parseFilename = filepath } . applyCPP

-- | Apply incredibly simplistic CPP handling. It only recognizes the following:
--
-- > #if FAY
-- > #ifdef FAY
-- > #ifndef FAY
-- > #else
-- > #endif
--
-- Note that this implementation replaces all removed lines with blanks, so
-- that line numbers remain accurate.
applyCPP :: String -> String
applyCPP =
    unlines . loop NoCPP . lines
  where
    loop _ [] = []
    loop state' ("#if FAY":rest) = "" : loop (CPPIf True state') rest
    loop state' ("#ifdef FAY":rest) = "" : loop (CPPIf True state') rest
    loop state' ("#ifndef FAY":rest) = "" : loop (CPPIf False state') rest
    loop (CPPIf b oldState') ("#else":rest) = "" : loop (CPPElse (not b) oldState') rest
    loop (CPPIf _ oldState') ("#endif":rest) = "" : loop oldState' rest
    loop (CPPElse _ oldState') ("#endif":rest) = "" : loop oldState' rest
    loop state' (x:rest) = (if toInclude state' then x else "") : loop state' rest

    toInclude NoCPP = True
    toInclude (CPPIf x state') = x && toInclude state'
    toInclude (CPPElse x state') = x && toInclude state'

-- | The CPP's parsing state.
data CPPState = NoCPP
              | CPPIf Bool CPPState
              | CPPElse Bool CPPState

-- | The parse mode for Fay.
parseMode :: ParseMode
parseMode = defaultParseMode
  { extensions = defaultExtensions
  , fixities = Just (preludeFixities ++ baseFixities)
  }

defaultExtensions :: [Extension]
defaultExtensions = map EnableExtension
  [EmptyDataDecls
  ,ExistentialQuantification
  ,FlexibleContexts
  ,FlexibleInstances
  ,GADTs
  ,ImplicitPrelude
  ,KindSignatures
  ,LambdaCase
  ,MultiWayIf
  ,NamedFieldPuns
  ,PackageImports
  ,RecordWildCards
  ,StandaloneDeriving
  ,TupleSections
  ,TypeFamilies
  ,TypeOperators
  ]
