{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Day19 (day19) where

import Util
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import Data.Map (Map)

type RangeMap :: Type
type RangeMap = Map String (Int, Int)

type RuleMap :: Type
type RuleMap = Map String [Rule]

day19 :: IO (Int, Int)
day19 = do
  (intervals, points) <- first solve <$> parse
  pure . both sum $
    ( map (sum . map snd) . filter (\a -> any (fits a) intervals) $ points
    , map (product . map (\(a, b) -> (b - a) + 1) . Map.elems) intervals
    )
 where
  fits :: [(String, Int)] -> RangeMap -> Bool
  fits part rangeMap =
    and [ n >= a && n <= b
        | (x, n) <- part
        , let (a, b) = rangeMap Map.! x
        ]

-- | From a map of all rules, create disjoint range maps containing intervals
-- for all variables that would be accepted. That is, if a part {x,m,a,s} is
-- inside one of the range maps, it is not in any other.
solve :: RuleMap -> [RangeMap]
solve rules = go (getRules "in") (fromList [([c], (1, 4000)) | c <- "xmas"])
 where
  getRules :: String -> [Rule]
  getRules = (rules Map.!)

  go :: [Rule] -> RangeMap -> [RangeMap]
  go []              _  = error "No rule could be applied: impossible"
  go (Reject   : _)  _  = []
  go (Accept   : _)  pt = [pt]
  go (Goto s   : _)  pt = go (getRules s) pt
  go (Rule{..} : rs) pt = case cond of
    Less i -> go newRules (changeR (lo, i - 1))    -- continue with good part
           ++ go rs (changeR (i, hi))              -- try next rule with bad part
    Greater i -> go newRules (changeR (i + 1, hi))
              ++ go rs (changeR (lo, i))
   where
    newRules    = getRules goto
    (lo, hi)    = pt Map.! part
    changeR val = Map.insert part val pt

-----------------------------------------------------------------------
-- Parsing

type Part :: Type
type Part = [(String, Int)]

type Comp :: Type
data Comp = Less Int | Greater Int

type Rule :: Type
data Rule
  = Rule { part :: String, cond :: Comp, goto :: String }
  | Accept
  | Reject
  | Goto String

parse :: IO (RuleMap, [Part])
parse = do
  (ins, ps) <- fmap (drop 2) . both lines . breakOn "\n\n" <$> readFile' "../inputs/day19.txt"
  pure ( fromList $ [("R", [Reject]), ("A", [Accept])] <> map (pInput pRule) ins
       , map (pInput pPart) ps
       )

pRule :: ReadP (String, [Rule])
pRule =
  (,) <$> munch1 isAlpha
      <*> ("{" *> ((pSingle <++ pLast) `sepBy1` ",") <* "}")
 where
  pSingle :: ReadP Rule
  pSingle = do
    part <- munch1 isAlpha
    op   <- get <&> \case
      '>' -> Greater
      _   -> Less
    cond <- op <$> (pNum <* ":")
    goto <- munch1 isAlpha
    pure Rule{..}

  pLast :: ReadP Rule
  pLast = munch1 isAlpha <&> \case
    "A" -> Accept
    "R" -> Reject
    s   -> Goto s

pPart :: ReadP Part
pPart = "{" *> (pPart' `sepBy` ",") <* "}"
 where
  pPart' :: ReadP (String, Int)
  pPart' = (,) <$> (munch1 isAlpha <* "=") <*> pNum
