module Day13
    ( day13     -- :: IO ()
    , day13Two  -- :: IO ()
    ) where

import Data.Attoparsec.Text (Parser)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

import qualified Data.Attoparsec.Text as A


day13 :: IO ()
day13 = interact $ tshow . go
  where
    go :: Text -> Int
    go input = abs
             . uncurry (*)
             . maximumBy (compare `on` snd)  -- max due to negative numbers

               -- We negate here because we care about numbers _bigger_
               -- than our arrival time
             . map (id &&& ((arrTime `mod`) . negate))
             $ busIDs
      where
        (arrTime, busIDs) :: (Int, [Int]) = inputWith pTimes input

day13Two :: IO ()
day13Two = interact $ tshow . chineseRemainder . snd . inputWith pTimesTwo

inputWith :: Parser [p] -> Text -> (Int, [p])
inputWith p (lines -> [x,y]) = (badlyParseInt x, fromRight [] $ A.parseOnly p y)
inputWith _ _                = error "Bad Input"

pTimes :: Parser [Int]
pTimes = catMaybes <$>
    (Just <$> A.decimal <|> A.char 'x' $> Nothing) `A.sepBy` A.char ','

pTimesTwo :: Parser [(Integer, Integer)]
pTimesTwo = filter ((/= 0) . snd) . zip [0, -1 ..] <$>
    (A.decimal <|> A.char 'x' $> 0) `A.sepBy` A.char ','
