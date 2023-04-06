module Day14 (day14) where

import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP hiding (many)
import Util

type CaveCoord :: Type
type CaveCoord = (Int, Int)

day14 :: IO (Int, Int)
day14 = do
  s <- parse
  let maxDepth = snd $ minimumBy (flip compare `on` snd) s
      solve f = length $ f maxDepth s Set.\\ s
  pure (solve solve1, solve solve2)
 where
  solve1 :: Int -> Set CaveCoord -> Set CaveCoord
  solve1 md = unfoldSet (rightToMaybe . fall md)

  solve2 :: Int -> Set CaveCoord -> Set CaveCoord
  solve2 md = unfoldSet isNew
   where
    isNew s = let pt = either id id (fall md s)
               in if pt `Set.member` s then Nothing else Just pt

parse :: IO (Set CaveCoord)
parse = Set.fromList
      . concatMap (concatMap (uncurry line) . (zip <*> tail) . pInput pLines)
      . lines
    <$> readFile "../inputs/day14.txt"
 where
  line :: CaveCoord -> CaveCoord -> [CaveCoord]
  line (a, b) (c, d) = [(x, y) | x <- [min a c .. max a c], y <- [min b d .. max b d]]

  pLines :: ReadP [CaveCoord]
  pLines = (((,) <$> (pNum <* ",") <*> pNum) `sepBy` " -> ") <* eof

fall :: Int -> Set CaveCoord -> Either CaveCoord CaveCoord
fall maxDepth cave = go (500, 0)
 where
  go pos = case fallStep pos of
    Nothing -> Right pos
    Just pt -> if snd pt > maxDepth then Left pt else go pt

  fallStep :: CaveCoord -> Maybe CaveCoord
  fallStep (x, y) = listToMaybe [ (a, y + 1)
                                | a <- [x, x - 1, x + 1]
                                , not ((a, y + 1) `Set.member` cave)
                                ]

unfoldSet :: forall a. Ord a => (Set a -> Maybe a) -> Set a -> Set a
unfoldSet f = go
 where
  go :: Set a -> Set a
  go !s = case f s of
    Just a' -> go (Set.insert a' s)
    Nothing -> s
