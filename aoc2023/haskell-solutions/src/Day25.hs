{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day25 (day25) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util

day25 :: IO (Int, String)
day25 = do
  inp <- readFile' "../inputs/day25.txt"
  pure (solve (parse inp) "xhk" "lsr", "Merry Christmas!")

solve :: Map String (Set String) -> [Char] -> [Char] -> Int
solve graph s e
  = length (bfs [one] (toList . (disconnectedGraph Map.!)))
  * length (bfs [two] (toList . (disconnectedGraph Map.!)))
 where
  edges@((one, two) : _) = fordFulkerson graph s e
  disconnectedGraph =
    foldl' (\acc (u, v) ->
              -- Delete edges
              Map.adjust (Set.delete v) u (Map.adjust (Set.delete u) v acc))
           graph
           edges

-- | The Ford–Fulkerson (or Edmonds–Karp) algorithm:
--
--     https://en.wikipedia.org/wiki/Ford%E2%80%93Fulkerson_algorithm
fordFulkerson :: Map String (Set String) -> String -> String -> [(String, String)]
fordFulkerson g s e = go g
 where
   existsPath :: (String -> [Item (Set String)]) -> Maybe [String]
   existsPath more = fmap reverse . find ((== e) . head) $
     bfsOn head [[s]] (\as@(a : _) -> map (: as) (more a))

   go :: Map String (Set String) -> [(String, String)]
   go graph = case existsPath neighbours of
     Nothing -> mapMaybe (\k -> do guard $ k `notElem` reachable
                                   n <- find (`elem` reachable) (graph Map.! k)
                                   pure (k, n))
                         (Map.keys graph)
      where
       reachable = Set.fromList (bfs [s] neighbours)
     Just path ->
       go (foldl' (\acc (u, v) ->
                      -- Delete u→v; we are in an unweighted graph,
                      -- what counts is just there being an edge.
                      Map.adjust (Set.delete v) u acc)
                  graph
                  (zip path (tail path)))
    where
     neighbours :: String -> [Item (Set String)]
     neighbours = toList . (graph Map.!)

parse :: String -> Map String (Set String)
parse = lines
    >>> map (\l -> let (a, b) = breakOn ":" l in (a, drop 1 (words b)))
    >>> concatMap (\(a, bs) -> (a, Set.fromList bs) : map (, Set.singleton a) bs)
    >>> Map.fromListWith (<>)
