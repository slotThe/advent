module Day8 (day8) where

import Util

day8 :: IO (Int, Int)
day8 = do
  inp <- parse
  let -- <|> for Maybe is precisely || for booleans, which is why we can
      -- take advantage of this here.
      visibles :: [Int] -> [Maybe Bool]
      visibles xs = map (\case True -> Just True; _ -> Nothing)
                  $ zipWith (>) xs (scanl' max minBound xs)
      one :: Int = length . catMaybes $ solve visibles (<|>) inp

      score :: [Int] -> [Int]
      score xs = [ maybe (length ys) succ (findIndex (>= y) ys) | (y : ys) <- tails xs]
      two :: Int = maximum $ solve score (*) inp

  pure (one, two)

-- | @solve method together xxs@ applies @method@ to every row and
-- column of @xxs@—rightwards and leftwards facing—separately, and zips
-- everything together with @together@.
solve :: forall a b. ([a] -> [b]) -> (b -> b -> b) -> [[a]] -> [b]
solve method together xxs = concat
            $ zipWith (zipWith together)
                      (map go xxs)                            -- horizontal
                      (transpose . map go . transpose $ xxs)  -- vertical
 where
  go :: [a] -> [b]
  go xs = zipWith together (method xs) (reverse . method . reverse $ xs)

parse :: IO [[Int]]
parse = map (map (read . (:[]))) . lines <$> readFile "../inputs/day8.txt"
