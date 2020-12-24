{-# LANGUAGE DeriveAnyClass #-}

module Day24
    ( day24     -- :: IO ()
    , day24Two  -- :: IO ()
    ) where

import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as HMap
import qualified Data.HashSet         as HSet


day24 :: IO ()
day24 = interact $ tshow . length . getTiles

day24Two :: IO ()
day24Two = interact $ tshow . length . (!! 100) . iterate gridStep . getTiles

-- | The "alive" tiles.
type Tiles = HashSet Hex

-- | Hexagon grids can be modelled quite elegantly by three coordinates,
-- see e.g.  <https://www.redblobgames.com/grids/hexagons/>
data Hex = Hex !Int !Int !Int
    deriving (Eq, Generic)
    deriving anyclass (Hashable)

-- | Directions we may go into
data Direction  = E | SE | SW | W | NW | NE
type Directions = [Direction]

-- | A single step a the game of ~~life~~ tiles.  This is basically an
-- easier version of 'Day17.gridStep'.
gridStep :: Tiles -> Tiles
gridStep hs = HMap.keysSet . HMap.filterWithKey live . HMap.fromListWith (+) $
    [(neighs, 1) | tile <- toList hs, neighs <- neighbours tile]
  where
    live :: Hex -> Int -> Bool
    live t n = n == 2 || n == 1 && t `HSet.member` hs

    neighbours :: Hex -> [Hex]
    neighbours tile = map (move tile) [E, SE, SW, W, NW, NE]

-- | Get the initial tile configuration.
getTiles :: Text -> Tiles
getTiles = maybe mempty (foldl' addTile mempty . map mkTile)
         . rightToMaybe
         . A.parseOnly (pTile `A.sepBy` A.endOfLine)
  where
    addTile :: Tiles -> Hex -> Tiles
    addTile hs tile
        | tile `HSet.member` hs = HSet.delete tile hs
        | otherwise             = HSet.insert tile hs

    mkTile :: Directions -> Hex
    mkTile = foldl' move (Hex 0 0 0)

-- | Some a tile in some direction
move :: Hex -> Direction -> Hex
move (Hex x y z) = \case
    E  -> Hex (x + 1) (y - 1) z
    SE -> Hex x       (y - 1) (z + 1)
    SW -> Hex (x - 1) y       (z + 1)
    W  -> Hex (x - 1) (y + 1) z
    NW -> Hex x       (y + 1) (z - 1)
    NE -> Hex (x + 1) y       (z - 1)

pTile :: Parser Directions
pTile = some $ A.choice
    [ "e"  $> E
    , "se" $> SE
    , "sw" $> SW
    , "w"  $> W
    , "nw" $> NW
    , "ne" $> NE
    ]
