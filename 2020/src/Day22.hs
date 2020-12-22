module Day22
    ( day22     -- :: IO ()
    , day22Two  -- :: IO ()
    ) where

import qualified Data.HashSet as HSet


day22 :: IO ()
day22 = interact $ tshow . playGame . getDecks

day22Two :: IO ()
day22Two = interact $ tshow . playGame2 . getDecks

-- | Cards are just numbers.
type Cards = [Int]

getDecks :: Text -> (Cards, Cards)
getDecks (map pPlayer . splitOn "\n\n" -> [a, b]) = (a, b)
getDecks _ = error "Only two players allowed"

-- | Play a game \o/
playGame :: (Cards, Cards) -> Int
playGame = calcWinner . uncurry go
  where
    go :: Cards -> Cards -> Cards
    go []     ys     = ys
    go xs     []     = xs
    go (x:xs) (y:ys)
        | x >= y    = go (xs <> [x, y]) ys
        | otherwise = go xs             (ys <> [y, x])

-- | There can only be one.
data Winner
    = Player1 !Cards
    | Player2 !Cards

-- | Play an explicitly recursive variant of the above game.
playGame2 :: (Cards, Cards) -> Int
playGame2 (c, c') = calcWinner case go c c' mempty of
    Player1 xs -> xs
    Player2 ys -> ys
  where
    go :: Cards -> Cards -> HashSet (Cards, Cards) -> Winner
    go xs       []       _    = Player1 xs
    go []       ys       _    = Player2 ys
    go a@(x:xs) b@(y:ys) hist =
        if | -- No infinite recursion
             (a, b) `HSet.member` hist -> Player1 a

             -- Go into subgame
           | x <= length xs && y <= length ys  ->
               case go (take x xs) (take y ys) mempty of
                   Player1{} -> p1Wins
                   Player2{} -> p2Wins

             -- Otherwise, normal rules apply
           | x >= y    -> p1Wins
           | otherwise -> p2Wins
      where
        p1Wins, p2Wins :: Winner
        p1Wins = go (xs <> [x, y]) ys             hist'
        p2Wins = go xs             (ys <> [y, x]) hist'

        hist' :: HashSet (Cards, Cards) = HSet.insert (a, b) hist

calcWinner :: [Int] -> Int
calcWinner = sum' . zipWith (*) [1..] . reverse

pPlayer :: Text -> [Int]
pPlayer (lines -> (_:xs)) = map badlyParseInt xs
pPlayer _                 = error "Invalid input"
