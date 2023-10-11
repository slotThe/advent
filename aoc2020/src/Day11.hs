module Day11
    ( day11     -- :: IO ()
    , day11Two  -- :: IO ()
    ) where

import Util hiding ((!?))
import Data.Vector ((!?))

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as Vec


day11 :: IO ()
day11 = T.interact $ tshow . countTakenSeats Adjacent 4 . toGrid

day11Two :: IO ()
day11Two = T.interact $ tshow . countTakenSeats Eye 5 . toGrid

-- | A seat is either empty, taken, or the floor (I suppose that's not
-- technically a seat then, but names are hard).
data Seat
    = Floor
    | Empty
    | Taken
    deriving (Eq)

-- | How to compute the neighbours of a seat.
data Rule
    = Adjacent
    | Eye

-- | The whole grid.
type Grid = Vector (Vector Seat)

toGrid :: Text -> Grid
toGrid = fmap (fromList . T.foldr ((:) . pSeat) []) . fromList . T.lines

countTakenSeats :: Rule -> Int -> Grid -> Int
countTakenSeats rule d = length . takenSeats . fixSeats rule d
  where
    takenSeats :: Grid -> Vector Seat
    takenSeats = Vec.concatMap (Vec.filter (== Taken))

-- | Observe seat configuration until no changes occur.
fixSeats
    :: Rule  -- ^ How to get neighbours
    -> Int   -- ^ When to die
    -> Grid
    -> Grid
fixSeats rule d seats =
    if seats == step then seats else fixSeats rule d step
  where
    -- | A single step where we update all cells.
    step :: Grid
    step = Vec.imap (\x row -> Vec.imap (\y seat -> updateElem (x, y) seat) row)
                    seats

    -- | Update the cell at @(x, y)@, according to the given conditions.
    updateElem :: (Int, Int) -> Seat -> Seat
    updateElem coords = \case
        Floor -> Floor
        Taken -> bool Taken Empty (die  coords)
        Empty -> bool Empty Taken (live coords)

    live :: (Int, Int) -> Bool
    live = (1 >) . neighbours

    die :: (Int, Int) -> Bool
    die = (d <=) . neighbours

    neighbours :: (Int, Int) -> Int
    neighbours = sum' . map (bool 0 1 . (== Taken)) . getNeighbours rule seats

-- | Get all neighbours of the cell at position @(x, y)@, according to
-- the given rules.
getNeighbours :: Rule -> Grid -> (Int, Int) -> [Seat]
getNeighbours rule seats (x, y) = mapMaybe strategy directions
  where
    strategy :: (Int, Int) -> Maybe Seat
    strategy = case rule of
        Adjacent -> getElem seats . bimap (x +) (y +)
        Eye      -> firstSeen seats (x, y)

    directions :: [(Int, Int)]
        = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a, b) /= (0, 0)]

-- | Get the first 'Seat' that a person sees.
firstSeen
    :: Grid
    -> (Int, Int)  -- ^ Index
    -> (Int, Int)  -- ^ (x-direction speed, y-direction speed)
    -> Maybe Seat
firstSeen seats (x, y) (xd, yd) = go xd yd
  where
    go :: Int -> Int -> Maybe Seat
    go a b = case getElem seats (x + a, y + b) of
        Nothing    -> Nothing
        Just Floor -> go (a + xd) (b + yd)
        Just seat  -> Just seat

-- | Safely index a 'Grid'.
getElem :: Grid -> (Int, Int) -> Maybe Seat
getElem vecs (x, y) = vecs !? x >>= (!? y)

pSeat :: Char -> Seat
pSeat = \case
    '.' -> Floor
    'L' -> Empty
    '#' -> Taken
    _   -> error "Input is not a valid seat."
