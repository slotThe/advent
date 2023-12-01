{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day01 (day01) where

import Util

day01 :: IO (Int, Int)
day01 = do
  input <- lines <$> readFile' "../inputs/day01.txt"
  let findNums repls = sum $ map (solve repls) input
  pure (findNums replacements1, findNums replacements2) -- 54331, 54518

solve :: [(String, Int)] -> String -> Int
solve repls inp =
  let first = findWork isPrefixOf (tails inp)
      last  = findWork isSuffixOf (reverse $ inits inp)
   in 10 * first + last
 where
  findWork :: (String -> String -> Bool) -> [String] -> Int
  findWork isIn = go
   where
    go []       = error "impossible"
    go (s : ss) = case find (\(a, _) -> a `isIn` s) repls of
      Nothing     -> go ss
      Just (_, b) -> b

replacements1 :: [(String, Int)]
replacements1 = [(show n, n) | n <- [1..9 :: Int]]

replacements2 :: [(String, Int)]
replacements2
   = replacements1
  <> [ ("one",   1)
     , ("two",   2)
     , ("three", 3)
     , ("four",  4)
     , ("five",  5)
     , ("six",   6)
     , ("seven", 7)
     , ("eight", 8)
     , ("nine",  9)
     ]
