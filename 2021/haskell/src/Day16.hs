{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day16 (day16) where

import Util hiding (many)
import Text.ParserCombinators.ReadP

day16 :: IO (Int, Int)
day16 = do
  packet <- head . pPackets . concatMap toHex <$> readFile "./puzzle-input/day16.txt"
  pure (addVersionNumbers packet, decode packet)  -- (984, 1015320896946)

type PacketVersion = Int
type TypeID        = Int

data Packet
  = Literal   { pv :: PacketVersion, val :: Int }
  | Operation { pv :: PacketVersion, typeID :: TypeID, subs :: [Packet] }
  deriving (Show)

addVersionNumbers :: Packet -> Int
addVersionNumbers = \case
    Literal   v _    -> v
    Operation v _ ps -> v + sum (map addVersionNumbers ps)

decode :: Packet -> Int
decode = \case
  Literal{val}            -> val              -- type-id 4
  Operation{typeID, subs} -> case typeID of
    0 -> sum     (map decode subs)
    1 -> product (map decode subs)
    2 -> minimum (map decode subs)
    3 -> maximum (map decode subs)
    5 -> if decode p1 >  decode p2 then 1 else 0
    6 -> if decode p1 <  decode p2 then 1 else 0
    7 -> if decode p1 == decode p2 then 1 else 0
    _ -> error ":<"
   where
    [p1, p2] = subs

pPackets :: String -> [Packet]
pPackets = fst . last . readP_to_S (many pPacket)

pPacket :: ReadP Packet
pPacket = do
  version <- pNumber 3
  typeID  <- pNumber 3
  if typeID == 4
    then Literal   version . fromBase2 . concat <$> pLiteralVal
    else Operation version typeID <$> pOperation

pLiteralVal :: ReadP [String]
pLiteralVal = (char '1' >> (:)    <$> pBinary 4 <*> pLiteralVal)
          <++ (char '0' >> (: []) <$> pBinary 4)

pOperation :: ReadP [Packet]
pOperation = (char '1' >> pNumber 11 >>= (`count` pPacket))
         <++ (char '0' >> pNumber 15 >>= fmap pPackets . (`count` get))

pBinary :: Int -> ReadP String
pBinary n = count n (char '1' <++ char '0')

pNumber :: Int -> ReadP Int
pNumber n = fromBase2 <$> pBinary n

toHex :: Char -> String
toHex = \case
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"
  _   -> ""
