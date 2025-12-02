{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Util

test :: Int -> Bool
test x = even n && (uncurry (==) $ (n `quot` 2) `splitAt` s)
 where s = show x; n = length s

test2 :: Int -> Bool
test2 x = n > 1 && any same [ chunksOf k s | k <- [1 .. (n `quot` 2)], n `mod` k == 0 ]
 where
  s = show x; n = length s
  same :: [String] -> Bool = \case
    []     -> True
    (x:xs) -> all (x ==) xs

main :: IO ()
main = do
  inp <- map (t . map (read @Int) . splitOn "-") . splitOn "," <$> readFile' "../inputs/day02.txt"
  print . sum $ map (sum . filter test  . uncurry enumFromTo) inp
  print . sum $ map (sum . filter test2 . uncurry enumFromTo) inp
 where
  t :: [a] -> (a, a)
  t [a,b] = (a,b)
