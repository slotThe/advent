{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day24 (day24) where

import Coords
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Util

type Spot :: Type
type Spot = Dim4

type Grid :: Type
type Grid = Map Coord [Spot]

day24 :: IO (Int, Int)
day24 = do
  grid <- parse
  let hi = maximum (Map.keys grid)
      start = C 0 (-1)
      stop  = below hi
      (a, g)  = solve grid hi start stop
      (b, g') = solve g    hi stop  start
      (c, _)  = solve g'   hi start stop
  pure (a, a + b + c)

solve :: Grid -> Coord -> Coord -> Coord -> (Int, Grid)
solve grid (C xmax ymax) start stop = go grid (Set.singleton start) 0
 where
  go :: Grid -> Set Coord -> Int -> (Int, Grid)
  go !g positions !n
    | stop `Set.member` positions = (n, g)
    | otherwise                   = go g' (walk g' positions) (n + 1)
        where g' = Map.fromListWith (<>) . concatMap (uncurry move) . toList $ g

  move :: Coord -> [Spot] -> [(Coord, [Spot])]
  move pt = map (mv &&& (:[]))
   where
    mv :: Spot -> Coord
    mv = coord . \case
      L -> left  pt
      R -> right pt
      U -> above pt
      D -> below pt

    coord :: Coord -> Coord
    coord (C a b) = C (a `mod` (xmax + 1)) (b `mod` (ymax + 1))

  walk :: Grid -> Set Coord -> Set Coord
  walk g poss = fromList [n | pos <- toList poss, n <- positions pos]
   where
    positions :: Coord -> [Coord]
    positions pt = [ neigh
                   | neigh@(C x y) <- pt : neighbours4 pt
                   , isNothing (g !? neigh)
                   , inGrid x y
                   ]

    -- Having to special case the start and end *sucks*, but this way we
    -- can completely forget about walls in the input, which makes the
    -- logic in 'move' a lot nicer.
    inGrid :: Int -> Int -> Bool
    inGrid x y = (x >= 0 && x <= xmax && y >= 0 && y <= ymax)
              || (x == 0 && y == -1)          -- start
              || (x == xmax && y == ymax + 1) -- stop

parse :: IO Grid
parse = Map.map pSpot . Map.filter (/= '.') . toCoordMap
      . map noWalls . noWalls . lines <$> readFile "../inputs/day24.txt"
 where
  noWalls :: [a] -> [a]
  noWalls = init . tail

  pSpot :: Char -> [Spot]
  pSpot = \case
    '<' -> [L]
    '>' -> [R]
    '^' -> [U]
    'v' -> [D]
