module Day2 (day2) where

import qualified Data.Map.Strict as Map
import Util

data Dir = Forward | Depth
  deriving (Eq, Ord)

-- >>> one <$> input
-- 1524750
one :: [(Dir, Int)] -> Int
one = foldl1' (*) . Map.elems . Map.fromListWith (+)

-- >>> two <$> input
-- 1592426537
two :: [(Dir, Int)] -> Int
two = (\(_, d, h) -> d * h)
    . foldl' (\(aim, depth, horiz) (d, i) -> case d of
                 Depth   -> (aim + i, depth, horiz)
                 Forward -> (aim, depth + aim * i, horiz + i))
             (0, 0, 0)

input :: IO [(Dir, Int)]
input = map (toDir . second read . word1) . lines
    <$> readFile "puzzle-input/day2.txt"
 where
  toDir :: (String, Int) -> (Dir, Int)
  toDir = \case
    ("forward", n) -> (Forward,   n)
    ("down"   , n) -> (Depth  ,   n)
    ("up"     , n) -> (Depth  , - n)
    _              -> error ":<"

day2 :: IO (Int, Int)
day2 = (one &&& two) <$> input
