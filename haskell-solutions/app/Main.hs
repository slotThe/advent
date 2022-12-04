import Day1
import Day3
import Day4
import Util

main :: IO ()
main = do
  day 1 day1
  day 3 day3
  day 4 day4

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "!!! Day " <> n <> " !!!"
  putStrLn $ "First  Task: " <> show one
  putStrLn $ "Second Task: " <> show two
  putStrLn ""
