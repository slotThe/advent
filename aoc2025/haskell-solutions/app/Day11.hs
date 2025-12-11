{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Util

import Data.HashMap.Strict qualified as M
import Data.HashMap.Strict (HashMap)
import Data.MemoTrie

type Search = ([(String, Bool)], String)

paths :: String -> String -> [String] -> HashMap String [String] -> Int
paths s e incls hm = memoFix go ([(i, False) | i <- incls], s)
 where -- HashMap as no instance for MemoTrie and I cba to write one.
  go :: (Search -> Int) -> Search -> Int
  go go (is, s)
    | s == e && all snd is = 1
    | otherwise = let is' = map (\(i, x) -> (i, x||(i==s))) is
                  in sum [ go (is', n) | n <- fromMaybe [] (hm M.!? s) ]

main :: IO ()
main = do
  inp <- M.fromListWith (<>) . map parse . lines <$> readFile' "../inputs/day11.txt"
  day 11 $ pure (paths "you" "out" [] inp, paths "svr" "out" ["fft", "dac"] inp) -- 511, 458618114529380
 where
  parse :: String -> (String, [String])
  parse (words -> (s:ss)) = (dropEnd 1 s, ss)
