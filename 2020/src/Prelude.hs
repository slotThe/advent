module Prelude
    ( module Exports
    , HashMap
    , Text
    , fromList
    , toList
    , tshow
    , breakOnDrop
    , badlyParseInt
    , textToChar
    , lines
    , unlines
    , interact
    ) where

import BasePrelude as Exports hiding (interact, lines, toList, unlines)

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Exts (fromList, toList)

tshow :: Show a => a -> Text
tshow = T.pack . show

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
