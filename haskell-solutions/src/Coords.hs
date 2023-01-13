module Coords (
  -- * Coords
  -- $coords
  Coord (..),
  toCoordMap,
  neighbours4,
) where

import Data.Map.Strict (Map)
import Util

{- $coords

2d coordinates: x grows to the *right* and y grows *down*.

-}

type Coord :: Type
data Coord = C { x :: Int, y :: Int }
  deriving stock (Show, Eq, Ord)

toCoordMap :: [[a]] -> Map Coord a
toCoordMap = fromList . prepMat (\i j el -> (C j i, el))
 where
  prepMat :: (Int -> Int -> a -> b) -> [[a]] -> [b]
  prepMat f = concat . zipWith (\i row -> zipWith (f i) [0..] row) [0..]

neighbours4 :: Coord -> [Coord]
neighbours4 (C x y) = [C (x + 1) y, C (x - 1) y, C x (y + 1), C x (y - 1)]
