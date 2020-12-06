module Prelude
    ( module Exports
    , HashMap
    , HashSet

    -- * Better {from,to}List
    , fromList
    , toList

    -- * Text!
    , Text
    , tshow
    , tread
    , textToChar
    , lines
    , unlines
    , interact
    , replace
    , splitOn
    , breakOnDrop
    , tryShow

    -- * Util
    , badlyParseInt
    , rightToMaybe
    , nubUnstable
    , sum'
    ) where

import BasePrelude as Exports hiding (interact, lines, toList, unlines)

import qualified Data.HashSet   as HSet
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Exts (fromList, toList)

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack

breakOnDrop :: Text -> Text -> (Text, Text)
breakOnDrop = (fmap (T.drop 1) .) . T.breakOn

badlyParseInt :: Text -> Int
badlyParseInt = fst . fromRight (0, "") . T.decimal

-- | Safe head with 'Text'.
textToChar :: Text -> Maybe Char
textToChar t = if T.null t then Nothing else Just $ T.head t

lines :: Text -> [Text]
lines = T.lines

unlines :: [Text] -> Text
unlines = T.unlines

interact :: (Text -> Text) -> IO ()
interact = T.interact

splitOn :: Text -> Text -> [Text]
splitOn = T.splitOn

replace :: Text -> Text -> Text -> Text
replace = T.replace

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
    Right r -> Just r
    _       -> Nothing

tryShow :: Show a => Maybe a -> Text
tryShow = maybe "Nothing :(" tshow

-- | Unstable sort in O(log₁₆ n)
nubUnstable :: (Eq a, Hashable a) => [a] -> [a]
nubUnstable = HSet.toList . HSet.fromList

-- | Proper sum
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldl' (+) 0
{-# INLINE sum' #-}
