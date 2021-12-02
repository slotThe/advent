module Util
    ( module BasePrelude
    , word1
    ) where

import BasePrelude

word1 :: String -> (String, String)
word1 = second (drop 1) . break isSpace
