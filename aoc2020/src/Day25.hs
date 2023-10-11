module Day25
    ( day25     -- :: IO ()
    , day25Two  -- :: IO ()
    ) where

import Util
import qualified Data.Text.IO as T
import qualified Data.Text    as T


day25 :: IO ()
day25 = T.interact $ tshow . encryptionKey . getPublicKeys

day25Two :: IO ()
day25Two = T.interact $ const "Merry Christmas!"

-- | The encryption key is simply the i-th element of one devices loop,
-- where i is the loop index of the other device.
encryptionKey :: (Int, Int) -> Int
encryptionKey (c, d) = loop c !! loopIndex d 7

-- | Get the 0-indexed (!) loop index.
loopIndex :: Int -> Int -> Int
loopIndex publicKey = length . takeWhile (/= publicKey) . loop

loop :: Int -> [Int]
loop subjectNumber = iterate' (\n -> (n * subjectNumber) `mod` 20_201_227) 1

getPublicKeys :: Text -> (Int, Int)
getPublicKeys (T.lines -> [c, d]) = (badlyParseInt c, badlyParseInt d)
getPublicKeys _                   = error "Only two public keys allowed!"
