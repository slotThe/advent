{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Day4 (day4) where

import Data.Kind (Type)
import Text.ParserCombinators.ReadP
import Util

-- | We want to use @"string"@ as a parser with @OverloadedStrings@
-- instead of having to write @string "string"@—make it so.
instance a ~ String => IsString (ReadP a) where
  fromString :: String -> ReadP a
  fromString = string

type RangePair :: Type
data RangePair = RangePair
  { elf11 :: Int
  , elf12 :: Int
  , elf21 :: Int
  , elf22 :: Int
  }

day4 :: IO (Int, Int)
day4 = do
  inp <- mapMaybe parse . lines <$> readFile "puzzle-input/day4.txt"
  let solve :: (RangePair -> Bool) -> Int
      solve by = length (filter by inp)
  pure (solve completelyOverlap, solve overlap)

overlap :: RangePair -> Bool
overlap RangePair{..} = not (elf12 < elf21 || elf22 < elf11)

completelyOverlap :: RangePair -> Bool
completelyOverlap RangePair{..}
  =  (elf21 <= elf11) && (elf12 <= elf22)
  || (elf11 <= elf21) && (elf22 <= elf12)

parse :: String -> Maybe RangePair
parse = fmap fst . listToMaybe . readP_to_S pRangePair
 where
  pRangePair :: ReadP RangePair
  pRangePair =
    RangePair <$> (num <* "-") <*> (num <* ",") <*> (num <* "-") <*> (num <* eof)

  num :: ReadP Int
  num = read <$> munch1 isDigit
