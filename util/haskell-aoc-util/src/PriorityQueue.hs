{-# LANGUAGE TypeFamilies, DerivingStrategies #-}
{- |
   Module      : PriorityQueue
   Description : Simple priority queue
   Copyright   : (c) Tony Zorman, 2024
   License     : AGPL
   Maintainer  : Tony Zorman <mail@tony-zorman.com>
-}
module PriorityQueue (PQueue(..), popMin, insert) where

import BasePrelude hiding (left, right, insert)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE

type PQueue :: Type -> Type
newtype PQueue a = PQ { unPQ :: IntMap (NonEmpty a) }
  deriving newtype (Show)

instance IsList (PQueue a) where
  type Item (PQueue a) = (Int, NonEmpty a)

  fromList :: [(Int, NonEmpty a)] -> PQueue a
  fromList = PQ . M.fromList

  toList :: PQueue a -> [(Int, NonEmpty a)]
  toList = M.toList . unPQ

popMin :: PQueue a -> Maybe (a, Int, PQueue a)
popMin (PQ pq) = do
  ((i, x :| xs), pq') <- M.minViewWithKey pq
  case NE.nonEmpty xs of
    Nothing -> Just (x, i, PQ pq')
    Just xs' -> Just (x, i, PQ $! M.insert i xs' pq')

insert :: a -> Int -> PQueue a -> PQueue a
insert k v (PQ pq) = PQ $! case pq M.!? v of
  Nothing -> M.insert v (NE.singleton k) pq
  Just{}  -> M.update (Just . (k NE.<|)) v pq
