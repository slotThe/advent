module Day12 (day12) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import BasePrelude

day12 :: IO (Int, Int)
day12 = parse <&> \i ->
  ( solve (\path x -> not $                        smallCave x && elem x path) i
  , solve (\path x -> not $ smallCaveTwice path && smallCave x && elem x path) i
  )

solve :: ([String] -> String -> Bool) -> Map String [String] -> Int
solve keep connections = length $ concatMap (go ["start"]) (connections Map.! "start")
 where
  go :: [String] -> String -> [[String]]
  go path kw
    | kw == "end" = [newPath]
    | otherwise   = concatMap (go newPath) downs
   where
    newPath :: [String] = kw : path  -- wrong direction, but this does not matter
    downs   :: [String] = filter (keep newPath) (connections Map.! kw)
{-# INLINE solve #-}

smallCave :: String -> Bool
smallCave = all isLower

smallCaveTwice :: [String] -> Bool
smallCaveTwice path = length smallPath /= length (nub smallPath)
 where
  smallPath :: [String] = filter smallCave path

parse :: IO (Map String [String])
parse = Map.map (delete "start")
      . Map.fromListWith union
      . concatMap ( (\(a, b) -> [(a, [b]), (b, [a])])
                  . second (drop 1) . break (== '-')  -- parse line
                  )
      . lines
    <$> readFile "puzzle-input/day12.txt"
