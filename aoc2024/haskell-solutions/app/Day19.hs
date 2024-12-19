import Util
import Data.MemoTrie

arrangements :: [String] -> String -> Int
arrangements towels = memoFix go
 where
  go :: (String -> Int) -> String -> Int
  go _  ""  = 1
  go go ptn = sum [ go (drop (length t) ptn) | t <- towels , t `isPrefixOf` ptn ]

main :: IO ()
main = do
  (towels, ptns) <- bimap (splitOn ", ") (lines . drop 2) . breakOn "\n\n"
               <$> readFile' "../inputs/day19.txt"
  let arrs = map (arrangements towels) ptns
  day 19 $ pure (length (filter (> 0) arrs), sum arrs)
