module Day4
    ( day4     -- :: IO ()
    , day4Two  -- :: IO ()
    ) where

import Util
import Data.Attoparsec.Text (Parser)
import Text.ParserCombinators.Perm ((<$$>), (<||>), permute)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T
import qualified Data.Text.Read       as T
import qualified Data.Text.IO as T


day4 :: IO ()
day4 = T.interact $ parseInputWith pPassport

day4Two :: IO ()
day4Two = T.interact $ parseInputWith pPassportTwo

parseInputWith :: Parser a -> Text -> Text
parseInputWith p
    = tshow
    . length
    . rights
    . map (A.parseOnly p . replace "\n" " ")
    . splitOn "\n\n"

-- | A single passport
data Passport = Passport
    { byr :: !Int
    , iyr :: !Int
    , eyr :: !Int
    , hgt :: !Height
    , hcl :: !Word16
    , ecl :: !Text
    , pid :: !Int
    , cid :: !(Maybe Text)
    }
    deriving (Show)

data Height
    = CM !Int
    | IN !Int
    deriving (Show)

-- | Permutation parser for passport, ignoring the values and only
-- checking if the key-value pair as a whole is there.  HACK
pPassport :: Parser Passport
pPassport = permute $
    Passport <$$> const 0      <$> parseField "byr"
             <||> const 0      <$> parseField "iyr"
             <||> const 0      <$> parseField "eyr"
             <||> const (CM 0) <$> parseField "hgt"
             <||> const 0      <$> parseField "hcl"
             <||>                  parseField "ecl"
             <||> const 0      <$> parseField "pid"
             <||> optional        (parseField "cid")

-- | Permutation parser for passport, with certain restrictions for the
-- validity of the fields.
pPassportTwo :: Parser Passport
pPassportTwo = permute $
    Passport <$$> pNumber 1920 2002 "byr"
             <||> pNumber 2010 2020 "iyr"
             <||> pNumber 2020 2030 "eyr"
             <||> parseFieldWith pHeight                "hgt"
             <||> parseFieldWith ("#" *> A.hexadecimal) "hcl"
             <||> parseFieldWith pEyeColor              "ecl"
             <||> parseFieldWith pPID                   "pid"
             <||> optional (parseField "cid")
  where
    -- | I could make this a separate sum type, but I'm too lazy for
    -- that right now x)
    pEyeColor :: Parser Text
        = A.choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

    pHeight :: Parser Height = do
        b <- A.decimal
        A.choice [ CM <$> inBounds b 150 193 <* "cm"
                 , IN <$> inBounds b 59  76  <* "in"
                 ]

    pPID :: Parser Int = do
        str <- A.takeWhile $ (&&) <$> (/= ' ') <*> (/= '\n')
        let parsedInt = fst <$> rightToMaybe (T.decimal str)

        whenAlt (9 == T.length str) $
            maybe empty pure parsedInt

pNumber :: Int -> Int -> Text -> Parser Int
pNumber l h lbl = parseFieldWith A.decimal lbl >>= \n -> inBounds n l h

-- | A sort of 'when' for the 'Alternative' type class.
whenAlt :: Alternative f => Bool -> f a -> f a
whenAlt predicate ret = if predicate then ret else empty

inBounds :: (Ord a, Alternative f) => a -> a -> a -> f a
inBounds n l h = whenAlt (n >= l && n <= h) $ pure n

parseField :: Text -> Parser Text
parseField = parseFieldWith (A.takeWhile (\c -> c /= ' ' && c /= '\n'))

parseFieldWith :: Parser a -> Text -> Parser a
parseFieldWith p key = A.string key *> ":" *> p <* A.skipSpace
