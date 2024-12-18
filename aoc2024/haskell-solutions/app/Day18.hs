{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}

import Util
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Coords as C
import Coords (Coord(..))
import qualified Debug.Trace

floodFill :: Set Coord -> Coord -> Coord -> Maybe Int
floodFill grid start goal = go mempty [(start, 0)]
 where
  go :: Set Coord -> [(Coord, Int)] -> Maybe Int
  go _    []              = Nothing
  go seen ((x, n) : xs)
    | x == goal           = Just n
    | x `Set.member` seen = go seen xs
    | otherwise           = go (Set.insert x seen)
                               (xs ++ [ (np, n + 1)
                                      | np <- C.neighbours4 x
                                      , np `Set.member` grid
                                      ])

binarySearch :: forall t. Integral t => (t -> Bool) -> t -> t -> t
binarySearch pred = go
 where
  go :: t -> t -> t
  go l r | l == r    = l
         | pred mid  = go l         mid
         | otherwise = go (mid + 1) r
   where mid = (r + l) `div` 2

main :: IO ()
main = do
  inp <- map ((\[a,b] -> C a b) . pInts) . lines <$> readFile' "../inputs/day18.txt"
  let grid = Set.fromList [ C x y | x <- [0..70], y <- [0..70] ]
      ff :: Int -> Maybe Int
        = \i -> floodFill (foldr Set.delete grid (take (i + 1) inp)) (C 0 0) (C 70 70)
  day 18 $ pure ( ff 1023, inp !! binarySearch (isNothing . ff) 1024 (length inp) )
