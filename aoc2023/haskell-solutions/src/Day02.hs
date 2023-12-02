module Day02 (day02) where

import Control.Applicative.Permutations
import Text.ParserCombinators.ReadP (eof, sepBy1)
import Util

type RGB :: Type
type RGB = (Int, Int, Int)

day02 :: IO (Int, Int)
day02 = do
  inp <- map (pInput (pLine <* eof)) . lines <$> readFile' "../inputs/day02.txt"
  pure (possibleSum inp, powerSum inp)

possibleSum :: [(Int, [RGB])] -> Int
possibleSum = sum . map fst . filter (not . any impossible . snd)
 where
  impossible :: RGB -> Bool
  impossible (r, g, b) = r > 12 || g > 13 || b > 14

powerSum :: [(Int, [RGB])] -> Int
powerSum = sum . map (max3 . unzip3 . snd)
 where
  max3 :: ([Int], [Int], [Int]) -> Int
  max3 (r,g,b) = maximum r * maximum g * maximum b

pLine :: ReadP (Int, [RGB])
pLine = (,) <$> ("Game " *> pNum <* ": ") <*> (pColours `sepBy1` "; ")
 where
  pColours :: ReadP RGB
  pColours = runPermutation $ (,,) <$> col " red" <*> col " green" <*> col " blue"
   where
    col :: ReadP String -> Permutation ReadP Int
    col p = toPermutationWithDefault 0 (pNum <* p <* optional ", ")
