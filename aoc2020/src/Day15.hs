module Day15
    ( day15     -- :: IO ()
    , day15Two  -- :: IO ()
    ) where

import Util
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Text    as T
import qualified Data.Text.IO as T


day15 :: IO ()
day15 = T.interact $ tshow . nth 2020 . pList

day15Two :: IO ()
day15Two = T.interact $ tshow . nth 30000000 . pList

pList :: Text -> [Int]
pList = map tread . T.splitOn ","

-- | Associated a number with the turn on which it was last seen.
type Numbers = HashMap Int Int

nth :: Int -> [Int] -> Int
nth stop xs = go startingNums 0 (len + 2)
  where
    len          :: Int     = length xs
    startingNums :: Numbers = fromList $ zip xs [1 .. len]

    go :: Numbers  -- ^ All last-seen numbers
       -> Int      -- ^ Element from __last__ turn
       -> Int      -- ^ Current turn
       -> Int
    go hm el !turn
        | turn == succ stop = el
        | otherwise         = case hm !? el of
            Nothing -> go insertLst 0                      nextTurn
            Just n  -> go insertLst (max (lastTurn - n) 1) nextTurn
      where
        insertLst :: Numbers = HMap.insert el lastTurn hm
        lastTurn  :: Int     = pred turn
        nextTurn  :: Int     = succ turn
