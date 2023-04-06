module Day3 (day3) where

import Data.Set qualified as Set
import Util

day3 :: IO (Int, Int)
day3 = do
  f <- lines <$> readFile "../inputs/day3.txt"
  pure (solve (map splitInTwo) f, solve (chunksOf 3) f)
 where
  splitInTwo :: [a] -> [[a]]
  splitInTwo s = chunksOf (length s `div` 2) s

solve :: (a -> [[String]]) -> a -> Int
solve prepare
  = sum
  . map (value . Set.findMax . foldr1 Set.intersection . map Set.fromList)
  . prepare

value :: Char -> Int
value c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1
