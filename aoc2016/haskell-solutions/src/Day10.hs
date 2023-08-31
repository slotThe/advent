{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day10 (day10) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Util

type Getter :: Type
data Getter = Bot Int | Output Int
  deriving stock (Show, Eq, Ord)

type Instruction :: Type
data Instruction = Val Int Getter | Give Int Getter Getter
  deriving stock (Show, Eq, Ord)

day10 :: IO (Maybe Getter, Int)
day10 = do
  res <- Map.toList . solve . pInput pLine <$> readFile "../inputs/day10.txt"
  let one = find (\(_, xs) -> case sort xs of
                   [17, 61] -> True
                   _        -> False)
                 res
  let two = product . map (head . snd) $
        filter (\case
                 (Output n, _) | n `elem` [0..2] -> True
                 _                          -> False)
               res
  pure (fst <$> one, two)

solve :: [Instruction] -> Map Getter [Int]
solve xs = memo
 where
  memo :: Map Getter [Int]
  memo = Map.fromListWith (<>) (concatMap doInstruction xs)

  doInstruction :: Instruction -> [(Getter, [Int])]
  doInstruction = \case
    Val n getter          -> [(getter, [n])]
    Give bot toLow toHigh ->
      let [lo, hi] = sort (memo Map.! Bot bot)
       in [(toLow, [lo]), (toHigh, [hi])]

pLine :: ReadP [Instruction]
pLine = (pValue <|> pIns) `endBy1` "\n" <* eof
 where
  pValue :: ReadP Instruction
  pValue = Val <$> ("value " *> pNum) <*> (" goes to " *> pOut)

  pIns :: ReadP Instruction
  pIns = Give <$> ("bot " *> pNum)
              <*> (" gives low to " *> pOut)
              <*> (" and high to " *> pOut)

  pOut :: ReadP Getter
  pOut = ("output " *> (Output <$> pNum))
     <++ ("bot " *> (Bot <$> pNum))
