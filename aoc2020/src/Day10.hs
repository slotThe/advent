module Day10
    ( day10     -- :: IO ()
    , day10Two  -- :: IO ()
    ) where

import qualified Data.List.NonEmpty as NE

import Data.List.NonEmpty (nonEmpty)


day10 :: IO ()
day10 = interact $ tshow . tryAdapters . getInput

day10Two :: IO ()
day10Two = interact $ tshow . (snd . NE.head <$>) . nonEmpty . count . getInput

getInput :: Text -> [Int]
getInput = sort . map badlyParseInt . lines

count :: [Int] -> [(Int, Int)]
count = foldl' go [(0, 1)]
  where
    go :: [(Int, Int)]  -- ^ [(list-elem, sub-total of combinations)]
       -> Int           -- ^ Current focus from the list
       -> [(Int, Int)]
    go list n = (n, sum totals) : list
      where
        -- | The sub-totals of all numbers that are "in range"
        totals :: [Int] = snd <$> takeWhile (\(i, _) -> i + 3 >= n) list

tryAdapters :: [Int] -> Maybe Int
tryAdapters = go 0 0 0                     -- Start focus: 0 (wall outlet)
  where
    go :: Int         -- ^ #(Gaps of size 1)
       -> Int         -- ^ #(Gaps of size 3)
       -> Int         -- ^ Current focus
       -> [Int]       -- ^ Rest list
       -> Maybe Int
    go !o !t _ []     = Just $ o * succ t  -- built in adapter
    go !o !t h (a:as) = case a - h of
        1 -> go (succ o) t        a as
        2 -> go o        t        a as
        3 -> go o        (succ t) a as
        _ -> Nothing
