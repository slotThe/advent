import BasePrelude

import Day2 (day2)

main :: IO ()
main = do
  day 2 day2

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "--- Day " <> n <> " ---"
  putStrLn $ "Part One: " <> show one
  putStrLn $ "Part Two: " <> show two
  putStrLn ""
