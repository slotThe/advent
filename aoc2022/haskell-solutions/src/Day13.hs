{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day13 (day13) where

import Text.ParserCombinators.ReadP hiding (many)
import Util

type Packet :: Type
data Packet = List [Packet] | El Int
 deriving stock (Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (El a)   (El b)   = compare a b
  compare (List a) (List b) = compare a b
  compare a@El{}   lb       = compare (List [a]) lb
  compare la       b@El{}   = compare la (List [b])

day13 :: IO (Int, Maybe Int)
day13 = do
  f <- filter (/= "") . lines <$> readFile "../inputs/day13.txt"
  let one = solve1 (parse1 f)
      two = solve2 (parse2 f)
  pure (one, two)

-----------------------------------------------------------------------

solve1 :: [(Packet, Packet)] -> Int
solve1 = sum . zipWith points [1..] . map (uncurry compare)
 where
  points :: Int -> Ordering -> Int
  points n o = if o == LT then n else 0

parse1 :: [String] -> [(Packet, Packet)]
parse1 = map ((\[x,y] -> (x, y)) . map (pInput pPacket)) . chunksOf 2

-----------------------------------------------------------------------

divide2, divide6 :: Packet
divide2 = List [List [El 2]]
divide6 = List [List [El 6]]

solve2 :: [Packet] -> Maybe Int
solve2 packets =
  (*) <$> elemIndex divide2 sortedPackets <*> elemIndex divide6 sortedPackets
 where sortedPackets = sort packets

parse2 :: [String] -> [Packet]
parse2 = ([divide2, divide6] <>) . map (pInput pPacket)

-----------------------------------------------------------------------

pPacket :: ReadP Packet
pPacket = (El <$> pNum) <++ (List <$> between "[" "]" (pPacket `sepBy` ","))
