{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DerivingStrategies #-}
import Util hiding (Op)

sim :: Int -> [Int] -> [Int -> Int -> Int] -> Bool
sim goal xs ops =
  goal `elem` foldl' (\acc x -> [e | a <- acc, o <- ops, let e = a `o` x, e <= goal])
                [head xs]
                (tail xs)

main :: IO ()
main = do
  inp <- map (fromJust . uncons . pInts) . lines <$> readFile' "../inputs/day07.txt"
  print $ sum [ a | (a, b) <- inp, sim a b [(+), (*)] ]
  print $ sum [ a | (a, b) <- inp, sim a b [(+), (*), \x y -> read (show x <> show y)] ]
