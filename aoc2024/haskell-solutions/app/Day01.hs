{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Util

main :: IO ()
main = do
  [l, r] <- map sort . transpose . map (mapMaybe (readMaybe . T.unpack) . T.splitOn " ")
         . T.lines <$> T.readFile "../inputs/day01.txt"
  day 1 . pure . both sum $
    ( zipWith (abs .: (-)) l r
    , [x * y | x <- l, let y = length (filter (x ==) r)]
    )
