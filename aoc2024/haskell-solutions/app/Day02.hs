{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ViewPatterns #-}
import Util

one :: [Int] -> Bool
one (zip <*> tail -> xs)
  = and $ (inOrder (>) || inOrder (<)) : [abs(a-b) `elem` [1..3] | (a,b) <- xs]
 where
  inOrder :: (Int -> Int -> Bool) -> Bool
    = \o -> all (uncurry o) xs

two :: [Int] -> Bool
two xs = any (one . (\i -> take (i-1) xs ++ drop i xs)) [1..length xs]

main :: IO ()
main = do
  inp <- map pInts . lines <$> readFile' "../inputs/day02.txt"
  traverse_ (print . length . flip filter inp) [one, two]
