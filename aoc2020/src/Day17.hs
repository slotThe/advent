{-# LANGUAGE DeriveAnyClass #-}

module Day17
    ( day17     -- :: IO ()
    , day17Two  -- :: IO ()
    ) where

import Util

import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet        as HSet
import qualified Data.Text.IO as T
import qualified Data.Text    as T


day17 :: IO ()
day17 = T.interact $ tshow . getStepDim 6 3

day17Two :: IO ()
day17Two = T.interact $ tshow . getStepDim 6 4

getStepDim :: Int -> Int -> Text -> Int
getStepDim n m = length . (!! n) . iterate (gridStep m) . pGrid . T.lines

-- | A four-dimensional vector with coordinates x, y, z, and w.
data Vec4 = Vec4 !Int !Int !Int !Int
    deriving (Eq, Generic, Show)
    deriving anyclass (Hashable)

gridStep :: Int -> HashSet Vec4 -> HashSet Vec4
gridStep n hs = HMap.keysSet . HMap.filterWithKey live . HMap.fromListWith (+) $
    [ (Vec4 (x + x') (y + y') (z + z') (w + w'), 1)
    | Vec4 x y z w     <- toList hs

    -- Get all real neighbours; the @<> [0]@ allows us to model a 3D
    -- vector as a 4D vector with zero in its @w@ component.
    , [x', y', z', w'] <- take 4 <$> (replicateM n [-1, 0, 1] <&> (<> [0]))
    , (x', y', z', w') /= (0, 0, 0, 0)
    ]
  where
    live :: Vec4 -> Int -> Bool
    live v neighs = neighs == 3 || neighs == 2 && v `HSet.member` hs

-- | Parse a list of lines into some grid of four-dimensional vectors.
-- Every vector start of not knowing anything about its neighbours
pGrid :: [Text] -> HashSet Vec4
pGrid lns = fromList
    [ Vec4 x y 0 0
    | (x, l  ) <- zip [0 ..] lns
    , (y, '#') <- zip [0 ..] (toList l)
    ]
