import Day1
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day11
import Day13
import Day14
import Util

main :: IO ()
main = do
  day 1 day1
  day 3 day3
  day 4 day4
  day 5 day5
  day 6 day6
  day 7 day7
  day 8 day8
  day 11 day11
  day 13 day13
  day 14 day14

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "!!! Day " <> n <> " !!!"
  putStrLn $ "First  Task: " <> show one
  putStrLn $ "Second Task: " <> show two
  putStrLn ""
