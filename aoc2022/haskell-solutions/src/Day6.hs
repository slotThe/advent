module Day6 (day6) where

import Util

day6 :: IO (Int, Int)
day6 = do
  f <- readFile "../inputs/day6.txt"
  pure (getStartOfMarker 4 f, getStartOfMarker 14 f)

getStartOfMarker :: Int -> String -> Int
getStartOfMarker n stream
  = (+ n) . length . takeWhile (\xs -> nub xs /= xs)
  -- Create sliding window of length n
  $ map (take n) (tails stream)
