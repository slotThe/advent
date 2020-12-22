module Day20
    ( day20     -- :: IO ()
    , day20Two  -- :: IO ()
    ) where

import Prelude
import Data.List.NonEmpty (nonEmpty)

import qualified Data.HashMap.Strict  as HMap
import qualified Data.Text            as T

day20 :: IO ()
day20 = interact $ tshow
                 . fmap getCorners
                 . traverse (fmap pTile . nonEmpty . lines)
                 . splitOn "\n\n"

day20Two :: IO ()
day20Two = undefined

data Tile = Tile
    { name      :: !Int
    , up        :: !Text
    , rightSide :: !Text
    , down      :: !Text
    , leftSide  :: !Text
    }
    deriving (Show)

type Frequencies = HashMap Int Int

-- | Corners are the tiles with exactly four (due to possible
-- flipped-ness) unique sides.
getCorners :: [Tile] -> Int
getCorners tiles = product' . map name . filter twoUniques $ tiles
  where
    twoUniques :: Tile -> Bool
        = (4 ==) . length . filter ((== Just 1) . (fs !?)) . getSides
    fs :: Frequencies = mkFrequencies tiles

-- | Account for flipped tiles.
getSides :: Tile -> [Int]
getSides Tile{ up, rightSide, down, leftSide } =
    map toNumber $ [id, T.reverse] <*> [up, rightSide, down, leftSide]
    -- Account for flipped tiles.
  where
    -- | This is such a horrible fold, but hey, it works
    toNumber :: Text -> Int
    toNumber = snd . T.foldr
        (\c (ix, s) -> (succ @Int ix, ) $
            s + 2^ix * case c of
                '#' -> 1
                '.' -> 0
                _   -> error "Bad input")
        (0, 0)

-- | Create a frequency map for every side.
mkFrequencies :: [Tile] -> Frequencies
mkFrequencies tiles =
    HMap.fromListWith (+) [(n, 1) | sides <- map getSides tiles, n <- sides]

pTile :: NonEmpty Text -> Tile
pTile (n :| ls) = Tile
    { name      = badlyParseInt . T.drop 1 . T.dropWhile (/= ' ') $ n
    , up        = head ls
    , down      = last ls
    , rightSide = fromList $ map T.last ls
    , leftSide  = fromList $ map T.head ls
    }
