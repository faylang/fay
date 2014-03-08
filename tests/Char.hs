module Char where

import FFI

import Data.Char

main :: Fay ()
main = do
  print 'a'
  putStrLn ('a' : "bc")
  print (head "abc")
  print (ord 'a')
  print (chr 97)
  print (chr (ord 'a'))
  print (ord (chr 97))
  print (isAscii 'a')
  print (isAscii (chr 128))
  print (isLatin1 (chr 255))
  print (isLatin1 (chr 256))
  print (toUpper 'a')
  print (toLower 'A')
  print (toLower (toUpper 'a'))
  print (isAsciiLower 'a')
  print (isAsciiLower 'A')
  print (isAsciiUpper 'Z')
  print (isAsciiUpper 'z')
  print (isAsciiUpper (toUpper 'a'))
  print (isAsciiUpper (toLower 'A'))
  print (isAsciiLower (toLower 'Z'))
  print (isAsciiLower (toUpper 'z'))
  print (all isDigit "0123456789")
  print (isDigit 'a')
  print (all isOctDigit "01234567")
  print (any isOctDigit ['8', '9'])
  print (all isHexDigit "0123456789ABCDEFabcdef")
  print (any isHexDigit "ghijklmnopqrstuvwxyzGHIJKLMNOPQRSTUVWXYZ")
  print (all isSpace [' ', '\t', '\n', '\r', '\f', '\v', '\xa0', '\x3000'])

