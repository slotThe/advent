{-# LANGUAGE NoImplicitPrelude #-}

import Data.Map.Strict qualified as Map
import Util

sim :: Int -> Int
sim n =
  let n'  = (n  `shiftL` 6 `xor` n ) .&. 0xffffff
      n'' = (n' `shiftR` 5 `xor` n') .&. 0xffffff in
  (n'' `shiftL` 11 `xor` n'') .&. 0xffffff

coagulate :: [Int] -> [([Int], Int)]
coagulate steps = nubOrdOn fst $ w4 (zipWith subtract ps (tail ps)) `zip` drop 4 ps
 where
  ps :: [Int]           = map (`mod` 10) steps
  w4 :: [Int] -> [[Int]] = map (take 4) . tails

main :: IO ()
main = do
  inp <- pInts <$> readFile' "../inputs/day22.txt"
  let sims = map (take 2001 . iterate' sim) inp
      one = sum $ map (!! 2000) sims
      two = maximum $ Map.fromListWith (+) (concatMap coagulate sims)
  day 22 $ pure (one, two)
