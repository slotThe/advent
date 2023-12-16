module Day16 (day16) where

import Control.Parallel.Strategies (parMap, rpar)
import Coords (Coord(..))
import qualified Coords as C
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Util

day16 :: IO (Int, Int)
day16 = do
  inp <- parse
  let xmax = V.length inp         - 1
      ymax = V.length (inp V.! 0) - 1

      one = [(C 0 0, R)]
      two = concatMap (\x -> [(C x 0, D), (C x ymax, U)]) [0..ymax]
         ++ concatMap (\y -> [(C 0 y, R), (C xmax y, L)]) [0..xmax]

      solve = maximum . parMap rpar (Set.size . Set.fromList . map fst . simulate inp)

  pure (solve one, solve two)

parse :: IO (Vector (Vector Char))
parse = V.fromList . map V.fromList . transpose . lines <$> readFile' "../inputs/day16.txt"

reflectFwd :: Coord -> Dim4 -> (Coord, Dim4)
reflectFwd beam = \case
  R -> (C.above beam, U)
  U -> (C.right beam, R)
  D -> (C.left  beam, L)
  L -> (C.below beam, D)

reflectBck :: Coord -> Dim4 -> (Coord, Dim4)
reflectBck beam = \case
  R -> (C.below beam, D)
  D -> (C.right beam, R)
  U -> (C.left  beam, L)
  L -> (C.above beam, U)

simulate :: Vector (Vector Char) -> (Coord, Dim4) -> [(Coord, Dim4)]
simulate grid start = bfs [start] \(beam, dir) ->
  filter (isJust . C.ix grid . fst) $
    case C.ix grid beam of
      Just '.'  -> [(C.move beam dir, dir)]
      Just '/'  -> [reflectFwd beam dir]
      Just '\\' -> [reflectBck beam dir]
      Just '-'  -> if dir `elem` [U, D]
                   then [(C.left beam, L), (C.right beam, R)]
                   else [(C.move beam dir, dir)]
      Just '|'  -> if dir `elem` [L, R]
                   then [(C.above beam, U), (C.below beam, D)]
                   else [(C.move beam dir, dir)]
      _ -> []
