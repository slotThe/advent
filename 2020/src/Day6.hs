module Day6
    ( day6     -- :: IO ()
    , day6Two  -- :: IO ()
    ) where

import qualified Data.HashSet as HSet


day6 :: IO ()
day6 = interact $ getAnswerWith HSet.unions

day6Two :: IO ()
day6Two = interact $ getAnswerWith (foldl1' HSet.intersection)

-- | Put everything into a 'HashSet', then clobber them together in the
-- right way and we have our solution.
getAnswerWith :: ([HashSet Char] -> HashSet Char) -> Text -> Text
getAnswerWith clobber
    = tshow
    . sum'
    . map (HSet.size . clobber . map (fromList . toList) . lines)
    . splitOn "\n\n"
