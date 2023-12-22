{-# OPTIONS_GHC -Wno-orphans #-}
module Util
    ( module BasePrelude
    , Type
    , Dim4(..)
    , day
    , both
    , word1
    , fromBase2
    , chunksOf
    , dropEnd
    , breakOn
    , rightToMaybe
    , pNum
    , pInput
    , bfs
    , bfsOn
    , nubInt
    , nubIntOn
    , nubOrd
    , nubOrdOn
    , converge
    , (.:)
    ) where

import BasePrelude hiding (left, right)
import Data.Containers.ListUtils (nubInt, nubIntOn, nubOrd, nubOrdOn)
import Data.Kind (Type)
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

type Dim4 :: Type
data Dim4 = U | R | D | L deriving stock (Show, Read, Eq, Ord)

day :: (Show a, Show b) => Int -> IO (a, b) -> IO ()
day (show -> n) res = do
  (one, two) <- res
  putStrLn $ "!!! Day " <> n <> " !!!"
  putStrLn $ "First  Task: " <> show one
  putStrLn $ "Second Task: " <> show two
  putStrLn ""

both :: Bifunctor p => (c -> d) -> p c c -> p d d
both f = bimap f f

word1 :: String -> (String, String)
word1 = second (drop 1) . break isSpace

fromBase2 :: String -> Int
fromBase2 = foldl' (\acc x -> 2 * acc + x) 0 . map digitToInt

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs
 where (as, bs) = splitAt n xs

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack | needle `isPrefixOf` haystack = ([], haystack)
breakOn _      []       = ([], [])
breakOn needle (x:xs)   = first (x:) $ breakOn needle xs

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

pInput :: ReadP p -> String -> p
pInput p = fst . head . readP_to_S p

pNum :: Read a => ReadP a
pNum = read <$> munch1 isDigit

-- | We want to use @"string"@ as a parser with @OverloadedStrings@
-- instead of having to write @string "string"@—make it so.
instance a ~ String => IsString (ReadP a) where
  fromString :: String -> ReadP a
  fromString = string

bfs :: forall a. Ord a => [a] -> (a -> [a]) -> [a]
bfs = bfsOn id

bfsOn :: forall a b. Ord b => (a -> b) -> [a] -> (a -> [a]) -> [a]
bfsOn trans start neighs = go mempty (fromList start)
 where
  go :: Set b -> Seq a -> [a]
  go !seen queue = case Seq.viewl queue of
    EmptyL    -> []
    a :< rest -> if a' `Set.member` seen
                 then go seen rest
                 else a : go (a' `Set.insert` seen)
                             (foldl' (|>) rest (neighs a))
     where a' = trans a

-- | Apply a function to an initial seed until it converges.
converge :: Eq a => (a -> a) -> a -> a
converge f a = fst . head . dropWhile (uncurry (/=)) . zip xs $ drop 1 xs
 where xs = iterate f a

-- | @f .: g ≡ λx y → f (g x y)@.
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)
