module Data.Char
  (chr
  ,ord
  ,isAscii
  ,isLatin1
  ,toUpper
  ,toLower
  ,isAsciiLower
  ,isAsciiUpper
  ,isDigit
  ,isOctDigit
  ,isHexDigit
  ,isSpace
  ) where

import Fay.FFI

chr :: Int -> Char
chr = ffi "String.fromCharCode(%1)"

ord :: Char -> Int
ord = ffi "%1.charCodeAt(0)"

isAscii :: Char -> Bool
isAscii c = c < '\x80'

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xff'

toUpper :: Char -> Char
toUpper = ffi "%1.toUpperCase()"

toLower :: Char -> Char
toLower = ffi "%1.toLowerCase()"

isAsciiLower :: Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isOctDigit :: Char -> Bool
isOctDigit c =  c >= '0' && c <= '7'

isHexDigit :: Char -> Bool
isHexDigit c  = isDigit c || c >= 'A' && c <= 'F' ||
                             c >= 'a' && c <= 'f'

isSpace :: Char -> Bool
isSpace = ffi "%1.replace(/\\s/g,'') != %1"
