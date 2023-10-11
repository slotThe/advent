module Day14
    ( day14     -- :: IO ()
    , day14Two  -- :: IO ()
    ) where

import Util
import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Text    as T
import qualified Data.Text.IO as T


day14 :: IO ()
day14 = T.interact $ tshow . memSum . pInput . T.lines

day14Two :: IO ()
day14Two = T.interact $ tshow . memSum' . pInput . T.lines

-- | An X symbolizes a clear or a set instruction.
data X = Set | Clear

-- | A bitmask is some number of indices and X instructions.
type Mask = [(Int, X)]

-- | An instruction is either a mask instruction or a memory
-- instruction.
data Instr
    = Mem (Integer, Integer)
    | Msk Mask

-- | The whole memory layout, as a map that maps memory locations to
-- values.
type MemoryLayout = HashMap Integer Integer

memSum :: [Instr] -> Integer
memSum = go [] mempty
  where
    go :: Mask -> MemoryLayout -> [Instr] -> Integer
    go _   hm []     = sum' $ HMap.elems hm
    go msk hm (x:xs) = case x of
        Msk msk'   -> go msk' hm  xs
        Mem (a, b) -> go msk  hm' xs
          where
            hm' :: MemoryLayout = HMap.insert a (applyMask b msk) hm

applyMask :: Bits b => b -> [(Int, X)] -> b
applyMask = foldl' decideMask
  where
    decideMask :: Bits b => b -> (Int, X) -> b
    decideMask num = \case
        (i, Set  ) -> num `setBit`   i
        (i, Clear) -> num `clearBit` i

memSum' :: [Instr] -> Integer
memSum' = go [] [] mempty
  where
    go :: Mask -> [Int] -> MemoryLayout -> [Instr] -> Integer
    go _   _    hm []     = sum' $ HMap.elems hm
    go msk flts hm (x:xs) = case x of
        Msk m      -> go m flts' hm xs
          where
            flts' :: [Int] = [0..35] \\ map fst m
        Mem (a, b) -> go msk flts hm' xs
          where
            hm' :: MemoryLayout
            hm' = foldl' (\hmap k -> HMap.insert k b hmap)
                         hm
                         (getAllKeys flts a msk)

getAllKeys
    :: Bits b
    => [Int]         -- ^ Indices of floating bits
    -> b             -- ^ Bit we are looking at for the index
    -> [(Int, X)] -- ^ Bitmask
    -> [b]           -- ^ Indices to change
getAllKeys floats = applyFloats .: foldl' decideMask
  where
    applyFloats :: Bits b => b -> [b]
    applyFloats num = map (foldl' flipBit num) allCombinations

    decideMask :: Bits b => b -> (Int, X) -> b
    decideMask num = \case
        (i, Set)   -> num `setBit` i
        (_, Clear) -> num

    flipBit :: Bits b => b -> (Int, Bool) -> b
    flipBit num (i, b) = if b then num `setBit` i else num `clearBit` i

    allCombinations :: [[(Int, Bool)]]
        = zip floats <$> getCombs (length floats)

    getCombs :: Int -> [[Bool]]
    getCombs 0 = [[]]
    getCombs i = ((True  :) <$> getCombs (i - 1))
              ++ ((False :) <$> getCombs (i - 1))

pInput :: [Text] -> [Instr]
pInput = mapMaybe (rightToMaybe . A.parseOnly pInstr)

pInstr :: Parser Instr
pInstr = Mem <$> pMem <|> Msk <$> pMask

pMem :: Parser (Integer, Integer)
pMem = (,) <$> ("mem[" *> A.decimal <* "] = ") <*> A.decimal

pMask :: Parser [(Int, X)]
pMask = go 0 [] . reverse . toList <$> ("mask = " *> A.takeText)
  where
    go :: Int -> Mask -> String -> Mask
    go _ l []     = l
    go i l (x:xs) = case x of
        '0' -> go (succ i) ((i, Clear) : l) xs
        '1' -> go (succ i) ((i, Set  ) : l) xs
        _   -> go (succ i) l                xs
