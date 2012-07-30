-- | Convert a Haskell value to a Fay value.

module Language.Fay.Show where

import           Data.List
import           Text.JSON
import           Text.Show.Pretty

-- | Convert a Haskell value to a Fay value. To read this in from JS,
-- use Fay.eval() which will evaluate the code in the context of Fay.
showToFay :: Show a => a -> String
showToFay a = case reify a of
  Nothing -> error $ "Unable to reify data type: " ++ show a
  Just a -> convert a

  where convert a = case a of
          Con "Date" [String s] -> "Fay$$date(" ++ encode (read s :: String) ++ ")"
          Con name [] -> name
          Con name values -> con name values
          Rec name [] -> name
          Rec name fields -> con name (map snd fields)
          Tuple xs -> "Fay$$tuple([" ++ intercalate "," (map convert xs) ++ "])"
          List xs -> "Fay$$list([" ++ intercalate "," (map convert xs) ++ "])"
          -- Not great:
          Neg v -> "-" ++ convert v
          Ratio x y -> convert x ++ "/" ++ convert y
          Integer x -> x
          Float x -> x
          Char x -> encode (read x :: Char)
          String x -> "Fay$$list(" ++ encode (read x :: String) ++ ")"

        con name values = foldr app name (reverse values) where
          app arg rest = rest ++ "(" ++ convert arg ++ ")"
