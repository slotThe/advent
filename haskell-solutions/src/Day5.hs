{-# LANGUAGE RecordWildCards #-}
module Day5 (day5) where

import GHC.Arr (readSTArray, writeSTArray, freezeSTArray, thawSTArray, STArray, elems, listArray)
import Text.ParserCombinators.ReadP hiding (many)
import Util

type Move :: Type
data Move = Move { move :: Int, from :: Int, to :: Int }

day5 :: IO (String, String)
day5 = do
  (a, b) <- parse
  let solve f = map head (applyMoves f a b)
  pure (solve reverse, solve id)

parse :: IO ([String], [Move])
parse = do
  (crates, instructions) <- bimap lines lines . breakOn "\n\n"
                        <$> readFile "puzzle-input/day5.txt"
  pure (pCrates crates, pIns instructions)
 where
  pCrates :: [String] -> [String]
  pCrates = map (filter (/= ' '))
          . transpose
          . mapMaybe (doParse (pLine <* eof)) . dropEnd 1

  pIns :: [String] -> [Move]
  pIns = mapMaybe (doParse pMove) . drop 2

  doParse :: ReadP p -> String -> Maybe p
  doParse p = fmap fst . listToMaybe . reverse . readP_to_S p

pLine :: ReadP String
pLine = choice [ "[" *> satisfy isUpper <* "]"  -- actual crate
               , " " *> get             <* " "  -- placeholder for alignment
               ]
        `sepBy1` " "

pMove :: ReadP Move
pMove = Move <$> ("move " *> pNum <* " ") <*> ("from " *> pNum <* " ") <*> ("to " *> pNum)

applyMoves :: (String -> String) -> [String] -> [Move] -> [String]
applyMoves adjust crates moves = runST $ do
  startArr <- thawSTArray $ listArray (1, length crates) crates
  elems <$> (freezeSTArray =<< foldM applyMove startArr moves)
 where
  applyMove :: STArray s Int String -> Move -> ST s (STArray s Int String)
  applyMove a Move{..} = do
    rowFrom <- readSTArray a from
    rowTo   <- readSTArray a to
    writeSTArray a from (drop move rowFrom)
    writeSTArray a to   (adjust (take move rowFrom) <> rowTo)
    pure a
