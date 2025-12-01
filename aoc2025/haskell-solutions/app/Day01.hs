{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Util

parse :: String -> Int
parse = \case
  ('L':s) -> - read s
  ('R':s) ->   read s

one :: [Int] -> Int
one = length . filter (== 0) . scanl' (\x y -> (x + y) `mod` 100) 50

two :: (Int, Int) -> Int -> (Int, Int)
two (z, d) n = (z + ((d' + abs n) `div` 100), (d + n) `mod` 100)
 where d' = ((signum n * d) `mod` 100)

main :: IO ()
main = do
  inp <- map parse . lines <$> readFile' "../inputs/day01.txt"
  day 1 $ pure (one inp, fst (foldl' two (0,50) inp))
