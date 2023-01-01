module Day20 (day20) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed (Vector, (!))
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Util

day20 :: IO (Int, Int)
day20 = do
  v <- V.fromList . zip [0..] . map read . lines <$> readFile "../inputs/day20.txt"
  let v2 = V.map (\(i, x) -> (i, x * 811589153)) v
  pure . both solve $ (mix v, iterate mix v2 !! 10)

-- I really don't want to write Vector instances.
type El :: Type
type El = (Int, Int)
initPos      , value :: El -> Int
initPos = fst; value = snd

solve :: Vector El -> Int
solve v = maybe 0 sumUp (V.findIndex ((== 0) . value) v)
 where
  sumUp :: Int -> Int
  sumUp i = ix 1000 + ix 2000 + ix 3000
   where ix j = value (v ! ((i + j) `mod` V.length v))

mix :: Vector El -> Vector El
mix input = runST do
  v <- V.thaw input
  traverse_ (step v) [0 .. V.length input - 1]
  V.freeze v
 where
  step :: forall m. PrimMonad m
       => MVector (PrimState m) El -> Int -> m (MVector (PrimState m) El)
  step v here = findEl >>= \case
    Nothing       -> pure v
    Just (ix, el) -> do
      let -- I don't know why this work, but it does!
          ix' = (ix + el) `mod` (MV.length v - 1)
      case compare ix ix' of
        LT -> MV.move (MV.slice ix        (ix' - ix) v) (MV.slice (ix + 1) (ix' - ix) v)
        GT -> MV.move (MV.slice (ix' + 1) (ix - ix') v) (MV.slice ix'      (ix - ix') v)
        EQ -> pure ()
      v <$ MV.write v ix' (here, el)
   where
    -- Return index and value of element that has 'here' as its index.
    findEl :: m (Maybe (Int, Int))
    findEl =
      foldM (\acc ix -> MV.read v ix <&> \el ->
                if initPos el == here then Just (ix, value el) else acc)
            Nothing
            [0 .. MV.length v - 1]
