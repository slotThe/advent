module Day8
    ( day8     -- :: IO ()
    , day8Two  -- :: IO ()
    ) where

import Prelude hiding ((!?))

import Data.Vector ((!?), (//))

import qualified Data.HashSet as HSet
import qualified Data.Text    as T


-- | WE SCREAM BECAUSE THIS IS ASSEMBLY
data Instruction
    = NOP Int
    | ACC Int
    | JMP Int
    deriving (Show)

type BootLoader a b = Either a b

type Aborted    = Int
type ReachedEnd = Int

day8 :: IO ()
day8 = interact $ tshow . runBootLoader . getInput

day8Two :: IO ()
day8Two = interact $ tshow . go
  where
    go :: Text -> Maybe ReachedEnd
    go = listToMaybe
       . mapMaybe (rightToMaybe . runBootLoader)
       . swapOutInstructions
       . getInput

getInput :: Text -> Vector Instruction
getInput = fromList . map badlyParseInput . lines

{- | Given some instructions, create almost identical variants thereof,
   only in each of the new ones either a 'NOP' has become a 'JMP' or the
   other way around.  Creates an exhaustive list of all possibilities.
-}
swapOutInstructions :: Vector Instruction -> [Vector Instruction]
swapOutInstructions instructions =
    insertInstruction . fmap trans <$> zip [0 ..] (toList instructions)
  where
    insertInstruction :: (Int, Instruction) -> Vector Instruction
        = \i -> instructions // [i]

    trans :: Instruction -> Instruction
        = \case
            JMP n -> NOP n
            NOP j -> JMP j
            a     -> a

-- | Run the boot loader.
runBootLoader :: Vector Instruction -> BootLoader Aborted ReachedEnd
runBootLoader instructions = go 0 0 (HSet.singleton 0)
  where
    go :: Int          -- ^ Index to check
       -> Int          -- ^ Accumulator
       -> HashSet Int  -- ^ Already seen indices
       -> BootLoader Aborted ReachedEnd
    go i !acc seen = case instructions !? i of
        Nothing -> Right acc
        Just x  -> case x of
            NOP{} -> tryInsert (succ i) acc
            ACC a -> tryInsert (succ i) (acc + a)
            JMP j -> tryInsert (i + j)  acc
      where
        tryInsert :: Int -> Int -> BootLoader Aborted ReachedEnd
        tryInsert newI newAcc
            | newI `HSet.member` seen = Left acc
            | otherwise               = go newI newAcc (HSet.insert newI seen)

badlyParseInput :: Text -> Instruction
badlyParseInput str =
    let (word, rest) = breakOnDrop " " str
        number = case T.head rest of
                     '+' -> tread $ T.tail rest
                     '-' -> tread rest
                     _   -> error "Bad input on number"
     in case word of
            "nop" -> NOP number
            "acc" -> ACC number
            "jmp" -> JMP number
            _     -> error "Bad input on instruction"
