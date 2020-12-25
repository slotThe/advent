module Day25
    ( day25     -- :: IO ()
    , day25Two  -- :: IO ()
    ) where

import Prelude hiding (loop)


day25 :: IO ()
day25 = interact $ tshow . encryptionKey . getPublicKeys

day25Two :: IO ()
day25Two = interact $ const "Merry Christmas!"

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
getPublicKeys (lines -> [c, d]) = (badlyParseInt c, badlyParseInt d)
getPublicKeys _                 = error "Only two public keys allowed!"
