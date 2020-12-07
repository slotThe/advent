module Day7
    ( day7     -- :: IO ()
    , day7Two  -- :: IO ()
    ) where

import Data.Attoparsec.ByteString (Parser)

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BS
import qualified Data.HashMap.Strict              as HMap


-- | Make type signatures read nicely \o/
type BagRules = HashMap ByteString [(Int, ByteString)]
type Bag      = ByteString
type Bags     = [ByteString]

day7 :: IO ()
day7 = BS.interact (BS.pack . show . go)
  where
    go :: Bag -> Int
    go input
        = length
        . filter (canContain bagrules "shiny gold")
        . HMap.keys
        $ bagrules
      where
        bagrules :: BagRules = getBagRules input

day7Two :: IO ()
day7Two = BS.interact \input ->
    BS.pack . show $ countBagsInside (getBagRules input) "shiny gold"

-- | Consistency assumption: Bags only appear once on the lhs of a rule.
-- This is very intuitive in the case of bag rules in an airport though.
getBagRules :: ByteString -> BagRules
getBagRules = HMap.unions . map (tryPLine . bsDropEnd 1) . BS.lines
  where
    -- | If an error happens during parsing everything that follows is
    -- garbage, so just error out.
    tryPLine :: ByteString -> BagRules
    tryPLine s = either error id  $ A.parseOnly pLine s

-- | Count the bags that have to be inside a given 'Bag', for it to
-- follow the 'BagRules'.
countBagsInside :: BagRules -> Bag -> Int
countBagsInside bags = go
  where
    go :: Bag -> Int
    go bag = maybe 0 (sum' . map \(n, b) -> n + n * go b) (bags !? bag)

-- | Given some rules, can a bag contain another bag?
canContain
    :: BagRules  -- ^ Rules
    -> Bag       -- ^ Can this bag be contained inside...
    -> Bag       -- ^ ...this bag
    -> Bool
canContain rules bag = go
  where
    go :: Bag -> Bool
    go bag' = case rules !? bag' of
        Nothing -> False
        Just rs -> (bag `elem` bags) || any go bags
          where
            bags :: Bags = map snd rs

-- | A full rule.
pLine :: Parser BagRules
pLine = HMap.singleton <$> pLhs <*> pNumBag `A.sepBy` A.char ','

-- | The left hand side of a rule.
pLhs :: Parser Bag
pLhs = do
    frst <- pWord
    scnd <- pWord
    void $ A.takeTill (A.inClass "0123456789")
    pure (frst <> " " <> scnd)

-- | A single 'content' of a bag.  One or more of these, separated by
-- commas, make up the rhs of a rule.
pNumBag :: Parser (Int, Bag)
pNumBag = A.skipSpace *> do
    n <- A.decimal <* A.skipSpace
    frst <- pWord
    scnd <- pWord
    void $ "bags" <|> "bag"
    pure (n, frst <> " " <> scnd)

pWord :: Parser ByteString
pWord = A.takeWhile1 (/= ' ') <* A.skipSpace

-- | O(1) because 'ByteString' is cool :)
bsDropEnd :: Int -> ByteString -> ByteString
bsDropEnd i b = BS.take (BS.length b - i) b
