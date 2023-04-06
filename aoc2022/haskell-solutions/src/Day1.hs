module Day1 (day1) where

import Data.Text    qualified as T
import Data.Text    (Text)
import Data.Text.IO qualified as T
import Util

day1 :: IO (Int, Int)
day1 = do
  f <- T.readFile "../inputs/day1.txt"
  pure (getNthMostWanted 1 f, getNthMostWanted 3 f)

getNthMostWanted :: Int -> Text -> Int
getNthMostWanted n = sum . take n . sortBy (flip compare) . parse
 where
  parse :: Text -> [Int]
  parse = map (sum . map (read . T.unpack) . T.lines)
        . T.splitOn "\n\n"
