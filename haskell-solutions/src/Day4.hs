{-# LANGUAGE RecordWildCards #-}
module Day4 (day4) where

import Text.ParserCombinators.ReadP
import Util

type RangePair :: Type
data RangePair = RangePair
  { elf11 :: Int
  , elf12 :: Int
  , elf21 :: Int
  , elf22 :: Int
  }

day4 :: IO (Int, Int)
day4 = do
  inp <- map parse . lines <$> readFile "../inputs/day4.txt"
  let solve :: (RangePair -> Bool) -> Int
      solve by = length (filter by inp)
  pure (solve completelyOverlap, solve overlap)

overlap :: RangePair -> Bool
overlap RangePair{..} = not (elf12 < elf21 || elf22 < elf11)

completelyOverlap :: RangePair -> Bool
completelyOverlap RangePair{..}
  =  (elf21 <= elf11) && (elf12 <= elf22)
  || (elf11 <= elf21) && (elf22 <= elf12)

parse :: String -> RangePair
parse = pInput pRangePair
 where
  pRangePair :: ReadP RangePair
  pRangePair = RangePair <$> (pNum <* "-") <*> (pNum <* ",") <*> (pNum <* "-") <*> (pNum <* eof)
