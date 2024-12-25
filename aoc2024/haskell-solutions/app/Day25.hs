{-# LANGUAGE NoImplicitPrelude #-}

import Util

main :: IO ()
main = do
  (locks, keys) <- both (map toVec) . partition (('#' ==) . head . head)
                . map lines . splitOn "\n\n" <$> readFile' "../inputs/day25.txt"
  let one = length [ () | l <- locks, k <- keys, all (5 >=) (zipWith (+) l k) ]
  guard (one == 3287)
  day 25 $ pure (one, "Merry Christmas!" :: String)
 where
  toVec :: [String] -> [Int]
  toVec xs = map (length . filter (== '#')) . transpose $ case xs of
    ("#####" : xs) -> xs
    xs             -> dropEnd 1 xs
