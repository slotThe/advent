module Util
    ( module BasePrelude
    , word1
    , fromBase2
    ) where

import BasePrelude

word1 :: String -> (String, String)
word1 = second (drop 1) . break isSpace

fromBase2 :: String -> Int
fromBase2 = foldl' (\acc x -> 2 * acc + x) 0 . map digitToInt
