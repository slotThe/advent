module Day21
    ( day21     -- :: IO ()
    , day21Two  -- :: IO ()
    ) where

import Prelude
import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as HMap
import qualified Data.HashSet         as HSet
import qualified Data.Text            as T


day21 :: IO ()
day21 = interact $ tshow . go . lines
  where
    go :: [Text] -> Int
    go inp = length $ filter (not . (`HSet.member` noAllergens)) allIngredients
      where
        (allIngredients, noAllergens) :: ([Text], HashSet Text)
            = (mconcat . HMap.elems) `second` buildMap inp

-- | God damn this looks funny
day21Two :: IO ()
day21Two
    = interact
    $ T.intercalate ","
    . concatMap snd
    . sortOn fst
    . toList
    . fmap toList
    . dangerousFoods
    . snd
    . buildMap
    . lines

type Ingredients = HashSet Text
type Allergens   = HashMap Text Ingredients

-- | Get all dangerous foods, i.e. the one that definitely contain an
-- allergen.
dangerousFoods :: Allergens -> Allergens
dangerousFoods = converge go
  where
    go :: Allergens -> Allergens
    go hm = singles <> HMap.map (`HSet.difference` singlesHS) hm
      where
        singles   :: Allergens   = HMap.filter ((== 1) . HSet.size) hm
        singlesHS :: Ingredients = mconcat . HMap.elems $ singles

-- | Get all ingredients that may contain an allergen; these are simply
-- the ones that are listed in more than one ingredient list.
buildMap :: [Text] -> ([Text], Allergens)
buildMap
    = second (fmap (foldl1' HSet.intersection))
    . foldl' (\(s, s') (m, m') -> (s <> m, HMap.unionWith (<>) s' m')) mempty
    . mapMaybe (rightToMaybe . A.parseOnly pIngredient)

-- | Associate an allergen with all ingredient lists that have it
-- listed, but keep these lists separate!
pIngredient :: Parser ([Text], HashMap Text [Ingredients])
pIngredient = do
    foods     <- T.words <$> A.takeWhile1 (/= '(')
    allergens <- pAllergens
    pure . (foods, ) . HMap.fromListWith (<>) $
        [(al, [fromList foods]) | al <- allergens]

pAllergens :: Parser [Text]
pAllergens = "(contains "
          *> A.takeWhile ((&&) <$> (/= ',') <*> (/= ')')) `A.sepBy` ", "
          <* ")"
