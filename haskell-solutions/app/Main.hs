import BasePrelude

main :: IO ()
main = print "hi!"

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "--- Day " <> n <> " ---"
  putStrLn $ "Part One: " <> show one
  putStrLn $ "Part Two: " <> show two
  putStrLn ""
