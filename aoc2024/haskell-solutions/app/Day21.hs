{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, TypeFamilies, DerivingStrategies  #-}

import Coords (Coord(..))
import Coords qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.MemoTrie (HasTrie, Reg, enumerate, enumerateGeneric, memo3, trie, trieGeneric, untrie, untrieGeneric, (:->:))
import Util

main :: IO ()
main = do
  inp  <- lines <$> readFile' "../inputs/day21.txt"
  let iter n = sum $ map (\s -> solve Key n s * read @Int (filter isDigit s)) inp
  day 21 $ pure (iter 2, iter 25)

w2 :: String -> [(Char, Char)]
w2 = zip <*> tail

type Mode :: Type
data Mode = Key | Dir deriving stock (Generic)

instance HasTrie Mode where
  newtype (Mode :->: b) = ModeTrie { unMode :: Reg Mode :->: b }
  trie = trieGeneric ModeTrie
  untrie = untrieGeneric unMode
  enumerate = enumerateGeneric unMode

solve :: Mode -> Int -> String -> Int
solve _ (-1) s = length s
solve m n    s = memo3 go m n s -- N.b.: memoFix will silently fail. That was fun to debug.
 where
  go :: Mode -> Int -> String -> Int
  go mode n = sum
            . map (\(b, e) -> minimum . map (solve Dir (n - 1)) $ paths mode b e)
            . w2 . ("A" <>)

paths :: Mode -> Char -> Char -> [String]
paths mode b e = maximumsBy (length . filter (uncurry (==)) . w2) $ go [(hm Map.! b, [])]
 where
  end = hm Map.! e
  (hm, no) = case mode of
    Key -> (keypad, C (-2) 0)
    Dir -> (dirpad, C (-2) (-1))

  go :: [(Coord, [Coord])] -> [String]
  go ps = case rs of
    []           -> ["A"]
    ((c, _) : _) -> if end /= c then go rs
                   else map ((<> "A") . map C.toChar . reverse . snd) rs
   where
    rs = [ (q, m : path)
         | (p, path) <- ps
         , m <- [C.east, C.west, C.north, C.south]
         , let q = p + m
         , no /= q
         , C.manhattan q end < C.manhattan p end
         ]

maximumsBy :: Ord b => (a -> b) -> [a] -> [a]
maximumsBy by xs = filter ((m ==) . by) xs
 where m = maximum $ map by xs

keypad :: Map Char Coord
keypad = Map.fromList
  [ ('7', C (-2) (-3)), ('8', C (-1) (-3)), ('9', C 0 (-3))
  , ('4', C (-2) (-2)), ('5', C (-1) (-2)), ('6', C 0 (-2))
  , ('1', C (-2) (-1)), ('2', C (-1) (-1)), ('3', C 0 (-1))
  ,                     ('0', C (-1) 0   ), ('A', C 0 0   )
  ]

dirpad :: Map Char Coord
dirpad = Map.fromList
  [                  ('^', C (-1) (-1)), ('A', C 0 (-1))
  , ('<', C (-2) 0), ('v', C (-1) 0)   , ('>', C 0 0)
  ]
