module Coords (
  -- * Coords
  -- $coords
  Coord (..),
  toCoordMap,

  -- * Neighbours
  neighbours4,

  -- * Moving
  above, below, left, right,
) where

import Data.Map.Strict (Map)
import Util

{- $coords

2d coordinates: x grows to the *right* and y grows *down*.

-}

type Coord :: Type
data Coord = C { x :: Int, y :: Int }
  deriving stock (Show, Eq, Ord)

instance Num Coord where
  (+) :: Coord -> Coord -> Coord
  C x y + C x' y' = C (x + x') (y + y')

  (-) :: Coord -> Coord -> Coord
  C x y - C x' y' = C (x - x') (y - y')

  (*) :: Coord -> Coord -> Coord
  C x y * C x' y' = C (x * x') (y * y')

  fromInteger :: Integer -> Coord
  fromInteger x   = C (fromInteger x) (fromInteger x)

  abs :: Coord -> Coord
  abs (C x y) = C (abs x) (abs y)

  -- PONDER: Complex number have a signum, but this doesn't really seem
  -- relevant.
  signum :: Coord -> Coord
  signum C{} = error "Coord: signum"

toCoordMap :: [[a]] -> Map Coord a
toCoordMap = fromList . prepMat (\i j el -> (C j i, el))
 where
  prepMat :: (Int -> Int -> a -> b) -> [[a]] -> [b]
  prepMat f = concat . zipWith (\i row -> zipWith (f i) [0..] row) [0..]

above, below, left, right :: Coord -> Coord
above (C x y) = C x      (y - 1)
below (C x y) = C x      (y + 1)
left  (C x y) = C (x - 1) y
right (C x y) = C (x + 1) y

neighbours4 :: Coord -> [Coord]
neighbours4 pt = map ($! pt) [right, left, below, above]
