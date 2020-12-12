module Day12
    ( day12     -- :: IO ()
    , day12Two  -- :: IO ()
    ) where

import Control.Lens ((*=), (+=), (-=), (.=), (<<.=))
import Control.Monad.Trans.State (State, evalState, gets)
import Data.Generics.Labels ()

import qualified Data.Text as T


day12 :: IO ()
day12 = interact $ tshow . moveShip . map pDirection . lines

day12Two :: IO ()
day12Two = interact $ tshow . moveShipTwo . map pDirection . lines

-- | __All__ valid moves.
data Move
    = N Int
    | S Int
    | E Int
    | W Int
    | F Int
    | R0
    | R90
    | R180
    | R270

-- | Direction the ship is moving in.
-- @ 0 = North, 90 = East, 180 = South, 270 = West @
newtype Direction = Direction { unDir :: Int }

-- | Moving the ship.
moveShip :: [Move] -> Int
moveShip = go 0 0 (Direction 90)
  where
    go :: Int        -- ^ y coordinate of the ship
       -> Int        -- ^ x coordinate of the ship
       -> Direction  -- ^ Direction we're moving in
       -> [Move]
       -> Int
    go ns ew _   []     = abs ns + abs ew
    go ns ew dir (x:xs) = case x of
        N i -> moveNorth i
        S i -> moveSouth i
        E i -> moveEast  i
        W i -> moveWest  i
        F i -> case unDir dir of
            0   -> moveNorth i
            90  -> moveEast  i
            180 -> moveSouth i
            _   -> moveWest  i  -- 270, assuming valid input
        R0   -> go ns ew dir        xs
        R90  -> go ns ew (addDir 90 ) xs
        R180 -> go ns ew (addDir 180) xs
        R270 -> go ns ew (addDir 270) xs
      where
        moveNorth, moveSouth, moveEast, moveWest :: Int -> Int
        moveNorth i = go (ns + i) ew dir xs
        moveSouth i = go (ns - i) ew dir xs
        moveEast  i = go ns (ew + i) dir xs
        moveWest  i = go ns (ew - i) dir xs

        addDir :: Int -> Direction
        addDir n = if   n `elem` [0, 90, 180, 270]
                   then Direction $ (unDir dir + n) `mod` 360
                   else dir

-- | Waypoint and ship coordinates.
data Coords = Coords
    { wp_x :: Int  -- ^ Waypoint North-South
    , wp_y :: Int  -- ^ Waypoint East-West
    , s_x  :: Int  -- ^ Ship North-South
    , s_y  :: Int  -- ^ Ship East-West
    } deriving (Generic)

moveShipTwo :: [Move] -> Int
moveShipTwo moves = go moves `evalState` Coords 1 10 0 0
  where
    go :: [Move] -> State Coords Int
    go []     = liftA2 (+) (abs <$> gets s_x) (abs <$> gets s_y)
    go (x:xs) = case x of
        N i  -> (#wp_x += i) *> go xs
        S i  -> (#wp_x -= i) *> go xs
        E i  -> (#wp_y += i) *> go xs
        W i  -> (#wp_y -= i) *> go xs
        F i  -> do (#s_x +=) . (i *) =<< gets wp_x
                   (#s_y +=) . (i *) =<< gets wp_y
                   go xs
        R0   -> go xs
        R90  -> do ns <- gets wp_x
                   ew <- #wp_y <<.= ns
                   (#wp_x .= -ew) *> go xs
        R180 -> (#wp_x *= -1) *> (#wp_y *= -1) *> go xs
        R270 -> do ns <- gets wp_x
                   ew <- #wp_y <<.= -ns
                   (#wp_x .= ew) *> go xs

pDirection :: Text -> Move
pDirection t
    | t == "R0"   || t == "L0"   = R0
    | t == "R90"  || t == "L270" = R90
    | t == "R180" || t == "L180" = R180
    | t == "R270" || t == "L90"  = R270
    | otherwise = case T.head t of  -- wildly unsafe, but hey, at least
                                    -- it's only during parsing :>
        'N' -> N i
        'S' -> S i
        'E' -> E i
        'W' -> W i
        'F' -> F i
        _   -> error "Bad input"
  where
    i :: Int = badlyParseInt (T.tail t)
