{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-operator-whitespace -Wno-type-defaults -Wno-incomplete-patterns #-}
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Set qualified as S
import Util

data C = C !Int !Int !Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

dist :: C -> C -> Int -- square distance is ok
dist (C a b c) (C d e f) = (a-d)^2 + (b-e)^2 + (c-f)^2

sim :: [C] -> [(C, C)] -> (Int, Int)
sim inp dsts =
  let h = H.fromList [(i, S.singleton i)|i<-inp]
      h1 = foldl' step h (take 1000 dsts)
      p1 = product . take 3 . sortOn Down . map S.size . nubOrd . H.elems $ h1
   in (p1, conv h1 (drop 1000 dsts))
 where
  conv :: HashMap C (Set C) -> [(C, C)] -> Int
  conv h (b:bs) =
    let h' = step h b
     in if all (`S.member` (h' H.! fst b)) (H.keys h')
        then uncurry (*) $ both (\(C x _ _) -> x) b
         else conv h' bs

  step :: HashMap C (Set C) -> (C, C) -> HashMap C (Set C)
  step h (b1, b2) =
    let s1 = h H.! b1
        s  = s1 `S.union` (h H.! b2)
    in if b2 `S.member` s1 then h
       else foldl' (\h x -> H.adjust (`S.union` s) x h) h s

main :: IO ()
main = do
  inp <- sort . map (t . pInts) . lines <$> readFile' "../inputs/day08.txt"
  let dists = sortOn (uncurry dist) [(x, y) | x <- inp, y <- inp, x > y]
  day 8 . pure $ sim inp dists -- 97384, 9003685096
 where
  t [a,b,c] = C a b c
