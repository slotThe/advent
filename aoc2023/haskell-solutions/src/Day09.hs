module Day09 (day09) where

import Util

day09 :: IO (Int, Int)
day09 = do
  inp <- map (map read . words) . lines <$> readFile' "../inputs/day09.txt"
  pure $ both (sum . map getHistVal) (inp, map reverse inp)

-- | Sum up the current last column to get the next history value. This works
-- in both directions because when reversing the list we also reverse the
-- signs when subtracting.
getHistVal :: [Int] -> Int
getHistVal =
  sum . map last . takeWhile (any (/= 0)) . iterate (\v -> zipWith subtract v (tail v))
