{-# LANGUAGE NoImplicitPrelude #-}

import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Util
import Data.Set qualified as Set
import Data.Set (Set)

main :: IO ()
main = do
  inp <- Map.fromListWith (<>)
      . concatMap ((\(k, v) -> [(k, Set.singleton v), (v, Set.singleton k)])
                   . second (drop 1) . breakOn "-")
      . lines <$> readFile' "../inputs/day23.txt"
  let one = Set.size $ Set.unions $ map (nCon inp 3) $ filter ("t" `isPrefixOf`) (Map.keys inp)
  let two = intercalate "," . toList $ maximumBy (compare `on` Set.size) $ maxClique inp
  guard (one == 926)
  guard (two == "az,ed,hz,it,ld,nh,pc,td,ty,ux,wc,yg,zz")
  day 23 $ pure (one, two)

nCon :: Map String (Set String) -> Int -> String -> Set (Set String)
nCon hm n = go n mempty
 where
  go :: Int -> Set String -> String -> Set (Set String)
  go 1 pth x = Set.singleton (x `Set.insert` pth)
  go n pth x = Set.unions $
    Set.map (go (n - 1) pth') (Set.filter ((pth' `Set.isSubsetOf`) . (hm Map.!)) vs)
   where
    vs   = hm Map.! x
    pth' = x `Set.insert` pth

maxClique :: Map String (Set String) -> Set (Set String)
maxClique hm = Set.map (\start -> go (Set.singleton start) (toList (Set.delete start ks))) ks
 where
  ks = Set.fromList (Map.keys hm)

  go :: Set String -> [String] -> Set String
  go clique []     = clique
  go clique (x:xs) = go next xs
   where
    next = if clique `Set.isSubsetOf` (hm Map.! x) then Set.insert x clique else clique
