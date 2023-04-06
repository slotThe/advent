module Day16
    ( day16     -- :: IO ()
    , day16Two  -- :: IO ()
    ) where

import Prelude
import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Text            as T


day16 :: IO ()
day16 = interact $ tshow
                 . sum'
                 . (\Document {nearbyTickets, fields} ->
                        mapMaybe (invalidTicket fields) nearbyTickets)
                 . getDocument

day16Two :: IO ()
day16Two = interact $ tshow . go
  where
    go :: Text -> Int
    go input
        = product
        . map ((myTicket !!) . fst)
        . filter (("departure" `T.isPrefixOf`) . name . snd)
        . allValid
        $ allMatches tickets' fields
      where
        Document{ myTicket, nearbyTickets, fields } = getDocument input

        tickets' :: [Ticket]
            = filter (isNothing . invalidTicket fields) nearbyTickets

allValid :: HashMap Int [Field] -> [(Int, Field)]
allValid = toList . fmap (head . toList) . converge go
  where
    go :: HashMap Int [Field] -> HashMap Int [Field]
    go m = (\f -> if length f == 1 then f else f \\ sLists m) <$> m
           -- Don't want to add something that's in a single list!
      where
        -- | All single lists.
        sLists :: HashMap Int [Field] -> [Field]
        sLists = concat . HMap.elems . HMap.filter ((== 1) . length)

allMatches :: [Ticket] -> [Field] -> HashMap Int [Field]
allMatches tickets
    = flipList
    . map (\f -> (f, ) $ mapMaybe (fieldMatches f tickets)
                                  [0 .. length (head tickets) - 1])

flipList ::  [(Field, [Int])] -> HashMap Int [Field]
flipList xs =
    HMap.fromListWith (++) $ [(y, [x]) | (a, b) <- xs, (x, y) <- (a, ) <$> b]

fieldMatches :: Field -> [Ticket] -> Int -> Maybe Int
fieldMatches Field{ ranges } tickets ix
    = bool Nothing (Just ix)
    . all (\a -> any (\(x1, x2) -> a >= x1 && a <= x2) ranges)
    $ map (!! ix) tickets

type Ticket = [Int]

data Field = Field
    { name   :: Text
    , ranges :: [(Int, Int)]
    }
    deriving (Show, Eq)

invalidTicket :: [Field] -> Ticket -> Maybe Int
invalidTicket fields = listToMaybe . mapMaybe (`inRange` fields)
  where
    inRange :: Int -> [Field] -> Maybe Int
    inRange a
        = bool (Just a) Nothing
        . any (any (\(x1, x2) -> a >= x1 && a <= x2) . ranges)

data Document = Document
    { fields        :: [Field]
    , myTicket      :: Ticket
    , nearbyTickets :: [Ticket]
    }
    deriving (Show)

getDocument :: Text -> Document
getDocument = fromRight (Document [] [] []) . A.parseOnly pDocument

pDocument :: Parser Document
pDocument =
    Document <$> (pField `A.sepBy1` A.endOfLine)
             <*> (pHeadline "your ticket:"    *> pTicket)
             <*> (pHeadline "nearby tickets:" *> (pTicket `A.sepBy1` A.endOfLine))

pField :: Parser Field
pField =
    Field <$> A.takeWhile1 (/= ':') <* ":" <* A.skipSpace
          <*> ((,) <$> A.decimal <*> ("-" *> A.decimal)) `A.sepBy1` " or "

pTicket :: Parser Ticket
pTicket = A.decimal `A.sepBy1` A.char ','

pHeadline :: Text -> Parser ()
pHeadline str = A.endOfLine *> A.endOfLine *> A.string str *> A.endOfLine
