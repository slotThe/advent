module Coords (
  -- * Coords
  -- $coords
  Coord (..),
  toCoordMap,

  -- * Neighbours
  neighbours4,

  -- * Moving
  above, below, left, right, move,

  -- * Unit lengths
  origin, north, south, east, west,

  -- * Indexing
  ix,

  -- * To and fro
  toChar, toPair,

  -- * Metrics
  manhattan,
) where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
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

toPair :: Coord -> (Int, Int)
toPair (C x y) = (x, y)

-- | Convert a unit coordinate to its respective character representation.
toChar :: Coord -> Char
toChar = \case
  C (-1) 0    -> '<'
  C 1    0    -> '>'
  C 0    (-1) -> '^'
  C 0    1    -> 'v'
  _           -> error "Coord.toChar: Want one of north, south, east, or west."

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

origin, north, south, west, east :: Coord
origin = C 0 0
north = above origin
south = below origin
west  = left  origin
east  = right origin

neighbours4 :: Coord -> [Coord]
neighbours4 pt = map ($! pt) [right, left, below, above]

-- | Manhattan distance between two coords.
manhattan :: Coord -> Coord -> Int
manhattan = (uncurry (+) . toPair . abs) .: (-)

-- | Move into the given direction.
move :: Coord -> Dim4 -> Coord
move pt = \case
  U -> above pt
  D -> below pt
  L -> left  pt
  R -> right pt

ix :: Vector (Vector a) -> Coord -> Maybe a
ix grid C{x, y} = grid V.!? x >>= (V.!? y)
