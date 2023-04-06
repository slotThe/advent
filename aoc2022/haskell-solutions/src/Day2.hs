module Day2 (day2) where

import Util hiding (Op)

type Me :: Type; type Op :: Type
data Me = X | Y | Z deriving stock (Read, Eq, Enum, Show)
data Op = A | B | C deriving stock (Read, Eq, Enum, Show)

day2 :: IO (Int, Int)
day2 = do
  games <- map (bimap read read . breakOn " ") . lines <$> readFile "../inputs/day2.txt"
  let solve :: (Op -> Me -> Me) -> Int
      solve f = sum $ map (\(op, me) -> result op (f op me)) games
  pure (solve (\_ b -> b), solve chooseOutcome)

result :: Op -> Me -> Int
result op me = (fromEnum me + 1)                               -- shape  score
             + 3 * ((fromEnum me - fromEnum op + 1) `mod` 3)   -- result score

chooseOutcome :: Op -> Me -> Me
chooseOutcome op me = toEnum $ (fromEnum op + fromEnum me - 1) `mod` 3
