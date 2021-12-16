import BasePrelude

import Day2  (day2)
import Day12 (day12)
import Day16 (day16)

main :: IO ()
main = do
  day 2  day2
  day 12 day12
  day 16 day16

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "--- Day " <> n <> " ---"
  putStrLn $ "Part One: " <> show one
  putStrLn $ "Part Two: " <> show two
  putStrLn ""
