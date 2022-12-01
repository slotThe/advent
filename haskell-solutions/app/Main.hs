import Day1
import Util

main :: IO ()
main = day 1 day1

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "!!! Day " <> n <> " !!!"
  putStrLn $ "First  Task: " <> show one
  putStrLn $ "Second Task: " <> show two
  putStrLn ""
