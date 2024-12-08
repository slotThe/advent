{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Util

main :: IO ()
main = do
  [l, r] <- map sort . transpose . map pInts . lines <$> readFile' "../inputs/day01.txt"
  day 1 . pure . both sum $
    ( zipWith (abs .: (-)) l r
    , [x * length (filter (x ==) r) | x <- l ]
    )
