{-# LANGUAGE RecordWildCards #-}
module Day11 (day11) where

import Control.Lens hiding ((:<), (|>))
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Text.ParserCombinators.ReadP hiding (many)
import Util

type Monkey :: Type
data Monkey = Monkey
  { startingItems :: Seq Int
  , operation     :: Int -> Int
  , compareWith   :: Int
  , outcomes      :: (Int, Int)
  , inspections   :: Int
  } deriving stock (Generic)

day11 :: IO (Int, Int)
day11 = do
  mons <- pMonkeys <$> parse
  let -- For all `a, n, m ∈ ℕ', we have that `a mod (n·m)' and `a' are
      -- congruent modulo `n' (and `m'); hence, we can introduce an
      -- artificial upper bound here.
      upper = product (map compareWith (Map.elems mons))
  pure (solve 20 (`div` 3) mons, solve 10000 (`mod` upper) mons)
 where
  solve :: Int -> (Int -> Int) -> Map Int Monkey -> Int
  solve rounds crush
    = product . take 2
    . map inspections . sortOn (Down . inspections)
    . Map.elems . (!! rounds) . iterate' (playRound crush)

playRound :: (Int -> Int) -> Map Int Monkey -> Map Int Monkey
playRound crush mons = foldl' turn mons [0 .. length mons - 1]
 where
  turn :: Map Int Monkey -> Int -> Map Int Monkey
  turn monkeys nr = go [] startingItems
   where
    Monkey{ startingItems, operation, compareWith, outcomes = (yes, no) }
      = monkeys Map.! nr

    go :: [(Int, Int)] -> Seq Int -> Map Int Monkey
    go qs vs = case Seq.viewl vs of
      i :< is ->
        let worry = crush (operation i)
            next  = if worry `mod` compareWith == 0 then yes else no
         in go ((next, worry) : qs) is
      EmptyL ->
        foldr (\(n, w) -> Map.adjust (#startingItems %~ (|> w)) n)
              (Map.adjust (\mon -> mon & #startingItems .~ mempty
                                       & #inspections   %~ (+ length qs))
                          nr
                          monkeys)
              qs

parse :: IO String
parse = readFile "../inputs/day11.txt"

pMonkeys :: String -> Map Int Monkey
pMonkeys = Map.fromList . zip [0 ..]
         . map (pInput go . T.unpack)
         . T.splitOn "\n\n" . T.pack
 where
  go :: ReadP Monkey
  go = do
    _ <- munch1 (/= '\n')
    startingItems <- Seq.fromList <$> pItems
    operation     <- pOp
    compareWith   <- skipSpaces *> "Test: divisible by " *> pNum
    outcomes      <- pOutcomes
    inspections   <- pure 0
    pure Monkey{..}

  pItems :: ReadP [Int]
  pItems = skipSpaces *> "Starting items: " *> (pNum `sepBy` ", ")

  pOp :: ReadP (Int -> Int)
  pOp = skipSpaces *> do
    _ <- "Operation: new = old "
    o <- ("* " $> (*)) <|> ("+ " $> (+))
    r <- ("old" $> 0) <|> pNum
    pure $ \old -> old `o` (if r == 0 then old else r)

  pOutcomes :: ReadP (Int, Int)
  pOutcomes = do
    t <- skipSpaces *> "If true: throw to monkey " *> pNum
    f <- skipSpaces *> "If false: throw to monkey " *> pNum
    pure (t, f)
