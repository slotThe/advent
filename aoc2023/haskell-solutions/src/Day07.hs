{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day07 (day07) where

import Util

type CardType :: Type
data CardType = HighCard | One | Two | Three | FullHouse | Four | Five
  deriving stock (Show, Eq, Ord, Enum)

type Hand :: Type
type Hand = (CardType, String)

day07 :: IO (Int, Int)
day07 = do
  inp <- readFile' "../inputs/day07.txt"
  pure ( solve pHand  "23456789TJQKA" inp
       , solve pHand2 "J23456789TQKA" inp
       )

solve :: (String -> Hand) -> [Char] -> String -> Int
solve p order
  = sum
  . zipWith (\n (_, i) -> n * i) [1..]
  . sortBy (compHands `on` fst)
  . map ((\[h, v] -> (p h, read v)) . words)
  . lines
 where
  compHands :: Hand -> Hand -> Ordering
  compHands (h1, s1) (h2, s2)
    | h1 == h2  = compare (map chars s1) (map chars s2)
    | otherwise = compare h1 h2

  chars :: Char -> Maybe Int
  chars n = elemIndex n order

pHand2 :: String -> Hand
pHand2 hand = (maximum (go hand), hand)
 where
  go h = case elemIndex 'J' h of
    Nothing -> [fst (pHand h)]
    Just ix -> let (prev, _ : after) = splitAt ix h
                in map (maximum . go . (\c -> prev ++ [c] ++ after))
                       "23456789TQKA"

pHand :: String -> Hand
pHand hand = (worth, hand)
 where
  worth :: CardType
  worth = case sortOn Down . map length . group . sort $ hand of
    [5]         -> Five
    (4 : _)     -> Four
    (3 : 2 : _) -> FullHouse
    (3 : _)     -> Three
    (2 : 2 : _) -> Two
    (2 : _)     -> One
    _           -> HighCard
