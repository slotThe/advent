module Day7 (day7) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.ParserCombinators.ReadP hiding (many)
import Util

{-
The trick is to recognise that we don't actually need the tree layout
that the question tries to push us into.  Neither the `ls' command
itself, nor the fact that it sometimes returns directories is of
interest to use here; we can simply treat everything as a flat list and
do things line by line.
-}

type Ins :: Type
data Ins = Cd String | File Integer | Ignore

day7 :: IO (Integer, Integer)
day7 = do
  m <- simulate <$> parse
  let one  = Map.foldl' (\acc v -> if v <= 100000 then acc + v else acc) 0 m
      need = 30000000 - (70000000 - (m Map.! []))
      two  = minimum . Map.elems $ Map.filter (>= need) m
  pure (one, two)

parse :: IO [Ins]
parse = mapMaybe (fmap fst . listToMaybe . readP_to_S pIns)
      . lines <$> readFile "../inputs/day7.txt"

pIns :: ReadP Ins
pIns = ("$ cd " *> (Cd <$> munch1 (pure True)))
   <++ (File <$> pNum)
   <++ pure Ignore

simulate :: [Ins] -> Map [String] Integer
simulate = snd . foldl' go ([], mempty)
 where
  go :: ([String], Map [String] Integer) -> Ins -> ([String], Map [String] Integer)
  go (path, tree) = \case
    Cd ".."   -> (tail path , tree)
    Cd dir    -> (dir : path, tree)
    File size -> (path, foldl' (\m k -> Map.insertWith (+) k size m) tree (tails path))
    _       -> (path, tree)
