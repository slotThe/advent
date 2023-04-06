module Day1
    ( day1     -- :: IO ()
    , day1Two  -- :: IO ()
    ) where

import qualified Data.HashMap.Strict as HMap
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

{- There is the fun one-liner

   @
     head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
   @

   that also works, but I felt particularly enthusiastic today and did
   some hash map based solution :)
-}

day1 :: IO ()
day1 = T.interact (showOutput . hmWith 0 . badlyParseInput)

day1Two :: IO ()
day1Two = T.interact go
  where
    go :: Text -> Text
    go input = showOutput
             . listToMaybe
             . mapMaybe (\a -> (a *) <$> hmWith a ints)
             $ ints
      where
         ints = badlyParseInput input

{- | Create a 'HashMap' and perform some arbitrary operations.

   Given some @n@, as well as a list @xs@, create a 'HashMap' out of the
   list and then check if there exist two numbers that sum up to
   @2020 - n@.  If that's the case, multiply them together.
-}
hmWith :: Int -> [Int] -> Maybe Int
hmWith n xs = uncurry (*) <$> listToMaybe hmap
  where
    hmap     :: [(Int, Int)]
        = toList . filterHM $ fromList [(x, 2020 - n - x) | x <- xs]
    filterHM :: HashMap Int Int -> HashMap Int Int
        = \hm -> HMap.filterWithKey (\_ v -> v `HMap.member` hm) hm

badlyParseInput :: Text -> [Int]
badlyParseInput = map badlyParseInt . T.lines

showOutput :: Maybe Int -> Text
showOutput = maybe "Nothing :(" tshow
