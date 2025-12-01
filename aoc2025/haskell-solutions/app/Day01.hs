{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Util

type Dir = Either Int Int

parse :: String -> Dir
parse = \case
  ('L':s) -> Left  (read s)
  ('R':s) -> Right (read s)

one :: [Dir] -> Int
one = length . filter (== 0) . scanl' (\x ey -> (x + d ey) `mod` 100) 50
 where d :: Dir -> Int = \case Left y -> -y; Right y -> y

two :: (Int, Int) -> Dir -> (Int, Int)
two (z, d) = \case
  Left  n -> let d' = ((-d)`mod`100) in
            (z + ((d' + n) `div` 100), (d - n) `mod` 100)
  Right n -> (z + ((d  + n) `div` 100), (d + n) `mod` 100)

main :: IO ()
main = do
  inp <- map parse . lines <$> readFile' "../inputs/day01.txt"
  day 1 $ pure (one inp, fst (foldl' two (0,50) inp))
