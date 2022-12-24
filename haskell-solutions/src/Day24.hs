{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day24 (day24) where

import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Util

type Coord :: Type
data Coord = Coord Int Int
  deriving stock (Eq, Ord)

type Spot :: Type
data Spot = L | R | U | D

type Grid :: Type
type Grid = Map Coord [Spot]

day24 :: IO (Int, Int)
day24 = do
  grid <- parse
  let m@(Coord x y) = maximum (Map.keys grid)
      start = Coord 0 (-1)
      stop  = Coord x (y + 1)
      (a, g)  = solve grid m start stop
      (b, g') = solve g    m stop  start
      (c, _)  = solve g'   m start stop
  pure (a, a + b + c)

solve :: Grid -> Coord -> Coord -> Coord -> (Int, Grid)
solve grid (Coord xmax ymax) start stop = go grid (Set.singleton start) 0
 where
  go :: Grid -> Set Coord -> Int -> (Int, Grid)
  go !g positions !n
    | stop `Set.member` positions = (n, g)
    | otherwise                   = go g' (walk g' positions) (n + 1)
        where g' = Map.fromListWith (<>) . concatMap (uncurry move) . toList $ g

  move :: Coord -> [Spot] -> [(Coord, [Spot])]
  move (Coord x y) = map (mv &&& (:[]))
   where
    mv :: Spot -> Coord
    mv = \case
      L -> coord (x - 1) y
      R -> coord (x + 1) y
      U -> coord x (y - 1)
      D -> coord x (y + 1)

    coord :: Int -> Int -> Coord
    coord a b = Coord (a `mod` (xmax + 1)) (b `mod` (ymax + 1))

  walk :: Grid -> Set Coord -> Set Coord
  walk g poss = fromList [n | pos <- toList poss, n <- positions pos]
   where
    positions :: Coord -> [Coord]
    positions (Coord x y) =
      [ Coord x' y'
      | (x', y') <- [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      , isNothing (g !? Coord x' y')
      , inGrid x' y'
      ]

    -- Having to special case the start and end *sucks*, but this way we
    -- can completely forget about walls in the input, which makes the
    -- logic in 'move' a lot nicer.
    inGrid :: Int -> Int -> Bool
    inGrid x y = (x >= 0 && x <= xmax && y >= 0 && y <= ymax)
              || (x == 0 && y == -1)          -- start
              || (x == xmax && y == ymax + 1) -- stop

parse :: IO Grid
parse = do
  inp <- map noWalls . noWalls . lines <$> readFile "../inputs/day24.txt"
  pure . fromList $ [ (Coord x y, pSpot el)
                    | (y, xrow) <- zip [0..] inp
                    , (x, el)   <- zip [0..] xrow
                    , el /= '.'
                    ]
 where
  noWalls :: [a] -> [a]
  noWalls = init . tail

  pSpot :: Char -> [Spot]
  pSpot = \case
    '<' -> [L]
    '>' -> [R]
    '^' -> [U]
    'v' -> [D]
