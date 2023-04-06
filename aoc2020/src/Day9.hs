module Day9
    ( day9     -- :: IO ()
    , day9Two  -- :: IO ()
    ) where

import Prelude hiding ((!?))
import Data.Vector ((!?))

import qualified Data.Vector as Vec


day9 :: IO ()
day9 = interact $ tshow . fmap snd . getElement . getInput

day9Two :: IO ()
day9Two = interact $ tshow . fmap sumEls . go . getInput
  where
    sumEls :: Vector Int -> Int
    sumEls l = minimum l + maximum l

    go :: Vector Int -> Maybe (Vector Int)
    go xs = getElement xs >>= \(ix, el) -> listToMaybe $
        mapMaybe (sumWindow (Vec.slice 0 (ix - 1) xs) el) [1 .. ix - 1]

getInput :: Text -> Vector Int
getInput = fromList . map badlyParseInt . lines

sumWindow :: Vector Int -> Int -> Int -> Maybe (Vector Int)
sumWindow xs s l = go l
  where
    go :: Int -> Maybe (Vector Int)
    go i | Vec.sum lst == s   = Just lst
         | i == Vec.length xs = Nothing
         | otherwise          = go (succ i)
      where
        lst :: Vector Int = Vec.slice (i - l) l xs

getElement :: Vector Int -> Maybe (Int, Int)
getElement xs = go 25
  where
    go :: Int -> Maybe (Int, Int)
    go i = xs !? i >>= \v -> if   v `elem` combs
                             then go (succ i)
                             else Just (i, v)
        where
          lst   :: Vector Int = Vec.slice (i - 25) 25 xs
          combs :: Vector Int = liftA2 (+) lst lst
