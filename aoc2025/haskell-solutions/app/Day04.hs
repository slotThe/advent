{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
import Util

import Data.Set qualified as S
import Data.Set (Set)

kill :: Set (Int, Int) -> Set (Int, Int)
kill set = S.fromList $
  [ (x, y)
  | (x, y) <- S.toList set
  , 4 < length (filter (`S.member` set) [ (x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1] ])
  ]

main :: IO ()
main = do
  inp <- S.fromList . catMaybes . concat
      . zipWith (\i row -> zipWith (\j x -> if x == '@' then Just (i, j) else Nothing) [0..] row)
                [0..]
                . lines <$> readFile' "../inputs/day04.txt"
  let solve f = S.size inp - S.size (f kill inp)
  day 4 $ pure (solve id, solve converge)
