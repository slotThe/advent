{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
import Util

ivalUnion :: [String] -> [(Int, Int)]
ivalUnion rs = zip s e
 where
  [s, e'] = transpose . sort . map (map (read @Int) . splitOn "-") $ rs
  e       = zipWith min (drop 1 s <> [maximum e' + 2]) (scanl1 max (map (+ 1) e'))

main :: IO ()
main = do
  (rs, ids) <- bimap ivalUnion (map (read @Int))
            . both lines . t . splitOn "\n\n" <$> readFile' "../inputs/day05.txt"
  day 5 $ pure ( length (filter (\x -> any (x `inIval`) rs) ids)
               , sum (map (\(x,y) -> y - x) rs)
               )
 where
  inIval :: Int -> (Int, Int) -> Bool
  inIval x (s, e) = s <= x && x < e

  t :: [a] -> (a, a)
  t [x, y] = (x, y)
