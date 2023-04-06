module Day23
    ( day23     -- :: IO ()
    , day23Two  -- :: IO ()
    ) where

import Prelude hiding (Vector)

import qualified Data.Vector.Unboxed         as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec

import Control.Monad.Primitive (PrimMonad(PrimState))
import Data.Vector.Unboxed (Vector, (!))
import Data.Vector.Unboxed.Mutable (MVector)

day23 :: IO ()
day23 = interact $ tshow . go 0 0 . Vec.map succ . playGame 100 [] . getNumbers
  where
    go :: Int -> Int -> Vector Int -> Int
    go !acc ix vec = case vec ! ix of
            0   -> acc  -- STOP RIGHT THERE CRIMINAL SCUM
            ix' -> go (acc * 10 + succ ix') ix' vec

day23Two :: IO ()
day23Two = interact $
    tshow . firstTwo . playGame 10_000_000 [10 .. 1_000_000] . getNumbers
  where
    firstTwo :: Vector Int -> Int
    firstTwo vec = succ one * succ two
      where
        one :: Int = vec ! 0
        two :: Int = vec ! one

getNumbers :: Text -> [Int]
getNumbers
    = map (read @Int) . sequence . (: []) . toList  -- Text → [Int]
    . head
    . lines

-- | Haskell is my favourite imperative language.
playGame :: Int -> [Int] -> [Int] -> Vector Int
playGame its adds xs = runST do
    let xs' = map pred (xs ++ adds)
        max = maximum xs'
        s   = head xs'

    {- Create starting vector; this simulates a cyclic list with a
    one-dimensional array in the usual way, i.e. 1 → 2 → 3 → 1 will be
    modelled as [2, 3, 1] ("the first index point to two, the second
    index points to three, the third index points to one").
    -}
    v <- MVec.new (length xs')
    traverse_ (uncurry (MVec.write v)) $ zip xs' (tail xs' ++ [head xs'])

    -- Iterate like there's no tomorrow
    foldM_ (const . go v max) s [1 .. its]
    Vec.unsafeFreeze v
  where
    go :: PrimMonad m
       => MVector (PrimState m) Int
       -> Int
       -> Int
       -> m Int
    go vec max start = do
        -- The three immediately adjacent
        one   <- MVec.read vec start
        two   <- MVec.read vec one
        three <- MVec.read vec two

        -- The new start = the fourth adjacent element
        newStart <- MVec.read vec three

        -- The destination
        let getDest :: Int -> Int
            getDest !a
                | a < 0                      = max
                | a `elem` [one, two, three] = getDest (a - 1)
                | otherwise                  = a
            dest = getDest if start < 0 then max else start - 1

        -- Shuffle things around in our cyclic list
        destNext <- MVec.read vec dest
        MVec.write vec start newStart
        MVec.write vec dest  one
        MVec.write vec three destNext

        pure newStart
