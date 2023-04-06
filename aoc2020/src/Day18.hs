module Day18
    ( day18     -- :: IO ()
    , day18Two  -- :: IO ()
    ) where

import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as A


day18 :: IO ()
day18 = interact $ tshow . exprSums pExpr

day18Two :: IO ()
day18Two = interact $ tshow . exprSums pExpr2

-- | Get the sum of all parsed expressions.
exprSums :: Parser Expr -> Text -> Int
exprSums p = sum' . map (sum . fmap evalExpr . A.parseOnly p) . lines

-- | A simple expression.
data Expr
    = Add Expr Expr
    | Mul Expr Expr
    | Num Int

evalExpr :: Expr -> Int
evalExpr = \case
    Add e e' -> evalExpr e + evalExpr e'
    Mul e e' -> evalExpr e * evalExpr e'
    Num i    -> i

-- | An expression with no operator precedences, read from left to
-- right.
pExpr :: Parser Expr
pExpr = exprWith pExpr `chainl1` pOp

-- | A left-to-right expression where addition has a higher precedence
-- than multiplication.
pExpr2 :: Parser Expr
pExpr2 = chainl1 (exprWith pExpr2 `chainl1` (pSymbol "+" $> Add))
                 (pSymbol "*" $> Mul)

-- | We still have to take care of parentheses.
exprWith :: Parser Expr -> Parser Expr
exprWith p =  A.char '(' *> p <* A.char ')'
          <|> Num <$> A.decimal

pOp :: Parser (Expr -> Expr -> Expr)
pOp =  pSymbol "+" $> Add
   <|> pSymbol "*" $> Mul

pSymbol :: Text -> Parser Text
pSymbol str = A.skipSpace *> A.string str <* A.skipSpace

-- | Get rid of left recursion in a left recursive grammar!
chainl1 :: forall a f. Alternative f => f a -> f (a -> a -> a) -> f a
chainl1 p op = p <**> rest
  where
    rest :: f (a -> a)
    rest =  (\f b g a -> g (f a b)) <$> op <*> p <*> rest
        <|> pure id
