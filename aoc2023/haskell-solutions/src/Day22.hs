{-# LANGUAGE RecordWildCards #-}
module Day22 (day22) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Set (Set)
import Data.Set qualified as Set
import Util

-- The order here is *very* important for the Ord instance.
type Brick :: Type
data Brick = Brick { z1 :: Int, z2 :: Int
                   , y1 :: Int, y2 :: Int
                   , x1 :: Int, x2 :: Int
                   }
 deriving stock (Show, Eq, Ord)

-- Falling bricks might occupy the space another brick used to have, so we
-- need to uniquely identify them :/. The brick comes first, so the Ord
-- instance checks it first.
type TaggedBrick :: Type
type TaggedBrick = (Brick, Int)

day22 :: IO (Int, Int)
day22 = do
  inp <- map (pInput pBrick) . lines <$> readFile' "../inputs/day22.txt"
  pure ( isStable (bool 0 1 .: (==))     inp
       , isStable (Set.size .: (Set.\\)) inp
       )

isStable :: (Set TaggedBrick -> Set TaggedBrick -> Int) -> [Brick] -> Int
isStable differences bricks = sum $
  parMap rseq
         (\(b, tag) -> let df = Set.delete (b, tag) finished
                        in differences df (simulate df))
         (Set.toList finished)
 where
  finished = simulate (Set.fromList (zip bricks [1..]))

simulate :: Set TaggedBrick -> Set TaggedBrick
simulate = converge (Set.fromList . foldl' go [])
 where
  go :: [TaggedBrick] -> TaggedBrick -> [TaggedBrick]
  go acc (b@Brick{z1, z2}, tag)
    | z1 == 1 = (b, tag) : acc
    | not $ any (\(b'@Brick{z2 = z2'}, _) -> z1 >= z2' && fallen ⋂ b') acc
    = go acc (fallen, tag) -- If nothing is below or everything below is disjoint, then fall.
    | otherwise = (b, tag) : acc
   where
    fallen = b{ z1 = z1 - 1, z2 = z2 - 1 }

(⋂) :: Brick -> Brick -> Bool
(Brick z11 z12 y11 y12 x11 x12) ⋂ (Brick z21 z22 y21 y22 x21 x22)
  =  max x11 x21 <= min x12 x22
  && max y11 y21 <= min y12 y22
  && max z11 z21 <= min z12 z22

pBrick :: ReadP Brick
pBrick = do
  x1 <- pNum <* "," ; y1 <- pNum <* "," ; z1 <- pNum <* "~"
  x2 <- pNum <* "," ; y2 <- pNum <* "," ; z2 <- pNum
  pure Brick{..}
