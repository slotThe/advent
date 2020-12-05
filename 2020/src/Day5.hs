module Day5
    ( day5     -- :: IO ()
    , day5Two  -- :: IO ()
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

import Data.List.NonEmpty (nonEmpty)


day5 :: IO ()
day5 = interact \input -> tryShow $
    maximum . NE.map (getSeatID . pBSP) <$> nonEmpty (lines input)

day5Two :: IO ()
day5Two = interact \input -> tryShow $
    getMySeat . NE.sort . NE.map (getSeatID . pBSP) <$> nonEmpty (lines input)

{- | I have to confess I used "Data.list.NonEmpty" solely because so I
   don't have to do a three-way (or two-way, using the implemented
   solution) pattern match here... :>

   The pattern matching stuff is probably more efficient (though I
   haven't looked at core and so I don't know how the zipping is
   actually done in this case), but this just looks so much *prettier*.
-}
getMySeat :: NonEmpty Int -> Maybe Int
getMySeat (x :| xs) =
    succ . fst <$> find (\(a, b) -> b /= succ a) (zip (x : xs) xs)

-- | I like making illegal states unrepresentable :)
data BSP = F | B | L | R
    deriving (Show, Read, Eq)

-- | Parse a string of the form "FBFFBFBFLLR" into a list of 'BSP', more
-- or less abusing the 'Read' instance of the latter.
pBSP :: Text -> [BSP]
pBSP = tread . wrap '[' ']' . T.intersperse ','
  where
    wrap :: Char -> Char -> Text -> Text
    wrap l r m = l `T.cons` m `T.snoc` r

-- | Do some binary search to get a SeatID.
getSeatID :: [BSP] -> Int
getSeatID seat = row * 8 + col
  where
    (bf, lr) = break (\a -> a == L || a == R) seat
    row      = binSearch (0, 127) bf
    col      = binSearch (0, 7  ) lr

    binSearch :: (Int, Int) -> [BSP] -> Int
    binSearch (s, _) []     = s
    binSearch (s, e) (x:xs) = case x of
        -- This code duplication is rather unfortunate
        F -> binSearch (s, pred newE) xs
        L -> binSearch (s, pred newE) xs
        B -> binSearch (newE, e)      xs
        R -> binSearch (newE, e)      xs
      where
        newE :: Int = e - ((e - s) `div` 2)
