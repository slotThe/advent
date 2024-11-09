module Day9 (day9) where

import Coords
import Util

day9 :: IO (Int, Int)
day9 = do
  inp <- parse
  let solve n = length . nubOrd $ simulate inp !! n
  pure (solve 1, solve 9)

simulate :: [Dim4] -> [[Coord]]
simulate = iterate' (scanl' follow (C 0 0)) . scanl' Day9.move (C 0 0)

follow :: Coord -> Coord -> Coord
follow t h@(C hx hy)
  | ax <= 1 && ay <= 1 = t
  | ax > ay            = C (hx - signum dx) hy
  | ay > ax            = C hx               (hy - signum dy)
  | otherwise          = C (hx - signum dx) (hy - signum dy)
 where
  C dx dy = h - t
  ax = abs dx
  ay = abs dy

move :: Coord -> Dim4 -> Coord
move pt = \case
  L -> left pt
  R -> right pt
  U -> below pt  -- this is correct
  D -> above pt  -- ditto

parse :: IO [Dim4]
parse = concatMap ((\(d, i) -> replicate (read i) (read d)) . word1)
      . lines <$> readFile "../inputs/day9.txt"
