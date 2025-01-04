{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-operator-whitespace -Wno-missing-local-signatures #-}
import Util
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  (ord, pgs) <- first (map t2) . t2 . map (map pInts . lines) . splitOn "\n\n"
            <$> readFile' "../inputs/day05.txt"
  let order = Map.fromListWith (<>) $ map (second Set.singleton . swap) ord
      a `before` b = a `Set.member` (order Map.! b)
      (good, bad) = partition (all (\(p, bs) -> all (`before` p) bs) . (zip <*> inits)) pgs
      solve = print . sum . map (\p -> p !! (length p `div` 2))
  solve good
  solve $ map (sortBy (bool LT GT .: before)) bad
 where
  t2 [a, b] = (a, b)
