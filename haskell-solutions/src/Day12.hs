module Day12 (day12) where

import Coords
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Util

day12 :: IO (Int, Int)
day12 = do
  input <- toCoordMap . lines <$> readFile "../inputs/day12.txt"
  let start :: Char -> [(Coord, Int)]
      start here = zip (Map.keys $ Map.filter (== here) input) (repeat 0)

      solve :: Char -> Int
      solve here = head [ len
                        | (pos, len) <- bfsOn fst (start here) (neighbours input)
                        , input Map.! pos == 'E'
                        ]

  pure (solve 'S', solve 'a')

neighbours :: Map Coord Char -> (Coord, Int) -> [(Coord, Int)]
neighbours look (pt, i) =
  [ (destPt, i + 1)
  | destPt  <- neighbours4 pt
  , destVal <- maybeToList $ look Map.!? destPt
  , 1 + elevation (look Map.! pt) >= elevation destVal
  ]
 where
  elevation :: Char -> Int
  elevation = ord . \case
    'S' -> 'a'
    'E' -> 'z'
    c   -> c
