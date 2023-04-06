module Prelude
    ( module Exports
    , HashMap
    , HashSet
    , ByteString
    , Vector
    , Hashable

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
    , product'
    , (!?)
    , (.:)
    , converge
    ) where

import BasePrelude as Exports hiding (interact, lines, toList, unlines)

import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet        as HSet
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Text.Read      as T

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Exts (fromList, toList)

tshow :: Show a => a -> Text
tshow = T.pack . show
{-# INLINE tshow #-}

tread :: Read a => Text -> a
tread = read . T.unpack
{-# INLINE tread #-}

breakOnDrop :: Text -> Text -> (Text, Text)
breakOnDrop = (fmap (T.drop 1) .) . T.breakOn
{-# INLINE breakOnDrop #-}

badlyParseInt :: Text -> Int
badlyParseInt = fst . fromRight (0, "") . T.decimal
{-# INLINE badlyParseInt #-}

-- | Safe head with 'Text'.
textToChar :: Text -> Maybe Char
textToChar t = if T.null t then Nothing else Just $ T.head t
{-# INLINE textToChar #-}

lines :: Text -> [Text]
lines = T.lines
{-# INLINE lines #-}

unlines :: [Text] -> Text
unlines = T.unlines
{-# INLINE unlines #-}

interact :: (Text -> Text) -> IO ()
interact = T.interact
{-# INLINE interact #-}

splitOn :: Text -> Text -> [Text]
splitOn = T.splitOn
{-# INLINE splitOn #-}

replace :: Text -> Text -> Text -> Text
replace = T.replace
{-# INLINE replace #-}

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
    Right r -> Just r
    _       -> Nothing
{-# INLINE rightToMaybe #-}

tryShow :: Show a => Maybe a -> Text
tryShow = maybe "Nothing :(" tshow
{-# INLINE tryShow #-}

-- | Unstable sort in O(n ∙ log₁₆ n)
nubUnstable :: (Eq a, Hashable a) => [a] -> [a]
nubUnstable = HSet.toList . HSet.fromList
{-# INLINE nubUnstable #-}

-- | Proper sum
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldl' (+) 0
{-# INLINE sum' #-}

-- | Proper product
product' :: (Num a, Foldable t) => t a -> a
product' = foldl' (*) 1
{-# INLINE product' #-}

(!?) :: (Eq k, Hashable k) => HashMap k v -> k -> Maybe v
(!?) hm k = HMap.lookup k hm
{-# INLINE (!?) #-}

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
{-# INLINE (.:) #-}

converge :: Eq a => (a -> a) -> a -> a
converge f x = if x == x' then x else converge f x'
  where x' = f x
{-# INLINE converge #-}
