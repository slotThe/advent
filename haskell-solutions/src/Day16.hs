module Day16 (day16) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP
import Util

day16 :: IO (Int, Int)
day16 = do
  inp <- parse
  let solve2 = Map.toList (solve inp 26)
  pure . bimap maximum maximum $
    ( solve inp 30
    , [ m + e
      | (me, m) <- solve2
      , (el, e) <- solve2
      , me `Set.intersection` el == mempty
      ]
    )

type Valve :: Type
data Valve = Valve
  { name     :: String
  , flowRate :: Int
  } deriving stock (Show, Eq, Ord)

solve :: Map Valve [Valve] -> Int -> Map (Set Valve) Int
solve m = go [((Valve "AA" 0, mempty), 0)]
 where
  go :: [((Valve, Set Valve), Int)] -> Int -> Map (Set Valve) Int
  go positions = \case
    0 -> getMax (map (first snd) positions)
    t -> go -- Weed out non-maximal pathways immediately.
            (Map.assocs (getMax (concatMap (move t) positions)))
            (pred t)

  move :: Int -> ((Valve, Set Valve), Int) -> [((Valve, Set Valve), Int)]
  move t ((pos, open), points)
    -- Move to neighbour...
    =  map (\neigh -> ((neigh, open), points)) (m Map.! pos)
    -- ...or open the current valve and count its *entire* contribution.
    <> [ ((pos, pos `Set.insert` open), pred t * flowRate pos + points)
       | pos `Set.notMember` open   -- closed
       , flowRate pos /= 0          -- non-trivial flow-rate
       ]

  getMax :: (Ord a, Ord b) => [(a, b)] -> Map a b
  getMax = Map.fromListWith max

parse :: IO (Map Valve [Valve])
parse = do
  vs <- map (pInput pValve) . lines <$> readFile "../inputs/day16.txt"
  pure $ Map.fromList
    [ (Valve n i, map (im Map.!) ns)
    | let im = Map.fromList [(m, Valve m j) | (m, j, _) <- vs]
    , (n, i, ns) <- vs
    ]

pValve :: ReadP (String, Int, [String])
pValve = do
  name   <- "Valve " *> pName
  num    <- " has flow rate=" *> pNum
  others <- choice
    [ "; tunnels lead to valves " *> (pName `sepBy` ", ")
    , "; tunnel leads to valve "  *> ((:[]) <$> pName)
    ]
  (name, num, others) <$ eof
 where
  pName :: ReadP [Char]
  pName = count 2 (satisfy isUpper)
