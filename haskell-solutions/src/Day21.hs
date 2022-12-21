{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
module Day21 (day21) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.ParserCombinators.ReadP
import Util hiding (Op)

day21 :: IO (Int, Int)
day21 = do
  mons <- fromList . map (pInput pMonkey) . lines <$> readFile "../inputs/day21.txt"
  pure (eval mons, humanNumber mons)

type Expr :: Type
data Expr = Leaf Int | Branch String Op String

type Op :: Type
data Op = Add | Sub | Mul | Div

evalOp :: Op -> Int -> Int -> Int
evalOp = \case
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

pMonkey :: ReadP (String, Expr)
pMonkey = (,) <$> (munch1 isLetter <* ": ") <*> (pOp <|> fmap Leaf pNum)
 where
  pOp :: ReadP Expr
  pOp = Branch <$> munch1 isLetter
               <*> choice [" + " $> Add, " - " $> Sub, " * " $> Mul, " / " $> Div]
               <*> munch1 isLetter

eval :: Map String Expr -> Int
eval monkeys = go (monkeys Map.! "root")
 where
  go :: Expr -> Int
  go = \case
    Leaf n        -> n
    Branch l op r -> evalOp op (go (monkeys Map.! l)) (go (monkeys Map.! r))

type Dir :: Type
data Dir = L | R

-- | To find out the number we ought to say, recurse down both paths
-- until we hit the "humn" value.  From there on out, invert all
-- operations that we see, in order to find out what we should do.  This
-- assumes that the "humn" value only occurs in one branch (and once) of
-- the "root" node.
humanNumber :: Map String Expr -> Int
humanNumber monkeys
  = let Branch lhs _ rhs = monkeys Map.! "root"
     in case (go lhs, go rhs) of
          (Left  f, Right n) -> f n
          (Right n, Left  f) -> f n
 where
  go :: String -> Either (Int -> Int) Int
  go = \case
    "humn" -> Left id
    k      -> case monkeys Map.! k of
      Leaf n        -> Right n
      Branch l op r -> case (go l, go r) of
        (Right n, Right m) -> Right $ evalOp op n m
        (Right n, Left  f) -> Left  $ f . reverseOp op L n
        (Left  f, Right n) -> Left  $ f . reverseOp op R n

  reverseOp :: Op -> Dir -> Int -> Int -> Int
  reverseOp op dir = case (op, dir) of
    (Add, _) -> flip (-)
    (Mul, _) -> flip div
    (Sub, L) -> (-)
    (Sub, R) -> flip (+)
    (Div, L) -> div
    (Div, R) -> flip (*)
