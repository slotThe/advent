module Day2
    ( day2     -- :: IO ()
    , day2Two  -- :: IO ()
    ) where

import Util
import qualified Data.Text     as T
import qualified Data.Text.IO  as T


day2 :: IO ()
day2 = T.interact (validatePWsWith passwordOK)
  where
    passwordOK :: PasswordDB -> Bool
    passwordOK PasswordDB{ letter, pos1, pos2, pw } =
        letterOccurs >= pos1 && letterOccurs <= pos2
      where
        letterOccurs :: Int = T.count (T.singleton letter) pw

day2Two :: IO ()
day2Two = T.interact (validatePWsWith tobogganPasswordOK)
  where
    tobogganPasswordOK :: PasswordDB -> Bool
    tobogganPasswordOK PasswordDB{ letter, pos1, pos2, pw } =
        letterOccursAt pos1 `xor` letterOccursAt pos2
      where
        letterOccursAt :: Int -> Bool
            = \pos -> Just letter == textToChar (T.drop (pos - 1) pw)

validatePWsWith :: (PasswordDB -> Bool) -> Text -> Text
validatePWsWith predicate =
    tshow . length . filter predicate . map badlyParseLine . T.lines

data PasswordDB = PasswordDB
    { letter :: !Char
    , pos1   :: !Int   -- ^ Min occurrences OR pos1
    , pos2   :: !Int   -- ^ Max occurrences OR pos2
    , pw     :: !Text
    }

-- | It hurts, but it works!  You may be thinking "it's probably time to use a
-- parser library" at this point, but you would be wrong.
badlyParseLine :: Text -> PasswordDB
badlyParseLine input =
    let (firstNumber , rest  ) = first badlyParseInt  $ breakOnDrop "-" input
        (secondNumber, rest' ) = first badlyParseInt  $ breakOnDrop " " rest
        (char        , passwd) = bimap T.head T.strip $ breakOnDrop ":" rest'
     in PasswordDB
            { letter = char
            , pos1   = firstNumber
            , pos2   = secondNumber
            , pw     = passwd
            }
