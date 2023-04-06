module Day3
    ( day3     -- :: IO ()
    , day3Two  -- :: IO ()
    ) where

import qualified Data.Text as T


day3 :: IO ()
day3 = interact (tshow . rightRDownD 3 1 . lines)

day3Two :: IO ()
day3Two = interact (tshow . go)
  where
    go input = r3d1 * r1d1 * r5d1 * r7d1 * r1d2
      where
        grid :: [Text] = lines input
        r3d1 :: Int    = rightRDownD 3 1 grid
        r1d1 :: Int    = rightRDownD 1 1 grid
        r5d1 :: Int    = rightRDownD 5 1 grid
        r7d1 :: Int    = rightRDownD 7 1 grid
        r1d2 :: Int    = rightRDownD 1 2 grid

{- | Move @r@ paces to the right and @d@ paces down in the grid.
Consistency assumption: The every line in the grid has equal width.
-}
rightRDownD
    :: Int     -- ^ How far to move right
    -> Int     -- ^ How far to move Down
    -> [Text]  -- ^ Grid
    -> Int
rightRDownD r d grid = go (drop d grid) r 0
  where
    w  :: Int = maybe 0 T.length (listToMaybe grid)

    go :: [Text]  -- ^ Grid
       -> Int     -- ^ Index to query
       -> Int     -- ^ Total trees encountered
       -> Int
    go gs i !ts = if   T.null h || length gs < d
                  then ts
                  else go (drop d gs) newIndex newTs
      where
        h        :: Text = fromMaybe "" (listToMaybe gs)
        newTs    :: Int  = if T.index h i == '#' then succ ts else ts
        newIndex :: Int  = (i + r) `mod` w
