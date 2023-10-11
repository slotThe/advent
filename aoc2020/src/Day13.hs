module Day13
    ( day13     -- :: IO ()
    , day13Two  -- :: IO ()
    ) where

import Util
import Data.Attoparsec.Text (Parser)
import Math.NumberTheory.Moduli.Chinese (chinese)
import Control.Arrow ((&&&))

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T
import qualified Data.Text    as T


day13 :: IO ()
day13 = T.interact $ tshow . go
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
day13Two = T.interact $ tshow . snd . fromJust . (\(p:ps) -> foldlM chinese p ps) . snd . inputWith pTimesTwo

inputWith :: Parser [p] -> Text -> (Int, [p])
inputWith p (T.lines -> [x,y]) = (badlyParseInt x, fromRight [] $ A.parseOnly p y)
inputWith _ _                  = error "Bad Input"

pTimes :: Parser [Int]
pTimes = catMaybes <$>
    (Just <$> A.decimal <|> A.char 'x' $> Nothing) `A.sepBy` A.char ','

pTimesTwo :: Parser [(Integer, Integer)]
pTimesTwo = filter ((/= 0) . snd) . zip [0, -1 ..] <$>
    (A.decimal <|> A.char 'x' $> 0) `A.sepBy` A.char ','
