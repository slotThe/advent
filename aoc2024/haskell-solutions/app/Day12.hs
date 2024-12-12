{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}
import Util hiding (Op)
import Coords
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

regionFor :: Ord a => (Coord, a) -> Map Coord a -> Set Coord
regionFor (xy, c) m = Set.fromList . map fst $ bfs [(xy, c)] \(p, c') ->
  [ (p', d) | p' <- neighbours4 p
            , d <- maybeToList (m Map.!? p')
            , d == c && c == c' ]

regionsForAll :: Ord a => Map Coord a -> [Set Coord]
regionsForAll m = go m []
 where
  go :: Ord a => Map Coord a -> [Set Coord] -> [Set Coord]
  go !m !r = case Map.lookupMin m of
    Nothing -> r
    Just p ->
      let rs = regionFor p m in
      go (Map.filterWithKey (\k _ -> Set.notMember k rs) m)
         (rs : r)

perim :: Set Coord -> Int
perim region = sum
  [ 1 | p <- toList region
      , n <- neighbours4 p
      , Set.notMember n region ]

sides :: Set Coord -> Int
sides region = inDir above + inDir below + inDir left + inDir right
 where
  inDir :: (Coord -> Coord) -> Int
  inDir dir = length . regionsForAll . Map.fromList . map (, ()) $
    mapMaybe (\p -> let p' = dir p in
                   if Set.notMember p' region then Just p' else Nothing)
             (toList region)

main :: IO ()
main = do
  inp <- regionsForAll . toCoordMap . lines <$> readFile' "../inputs/day12.txt"
  traverse_ (\f -> print $ sum $ map (\r -> Set.size r * f r) inp)
            [perim, sides]
