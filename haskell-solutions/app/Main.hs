import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day11
import Day12
import Day13
import Day14
import Day16
import Day20
import Day21
import Day24
import Util

main :: IO ()
main = do
  day 1 day1
  day 2 day2
  day 3 day3
  day 4 day4
  day 5 day5
  day 6 day6
  day 7 day7
  day 8 day8
  day 11 day11
  day 13 day13
  day 12 day12
  day 14 day14
  day 16 day16
  day 20 day20
  day 21 day21
  day 24 day24

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "!!! Day " <> n <> " !!!"
  putStrLn $ "First  Task: " <> show one
  putStrLn $ "Second Task: " <> show two
  putStrLn ""
