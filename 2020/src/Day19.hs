module Day19
    ( day19     -- :: IO ()
    , day19Two  -- :: IO ()
    ) where

import Data.Attoparsec.Text (Parser)

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Text            as T


day19 :: IO ()
day19 = interact $ tshow . allMatches . pInput

day19Two :: IO ()
day19Two = interact $ tshow . length . allMatches . first rules' . pInput
  where
    rules' :: RuleMap -> RuleMap
    rules' rules =
        foldl' (\m (k, v) -> HMap.insert k v m)
               rules
               [ (8 , MustMatch [[42]    , [42, 8]     ])  -- 42⁺
               , (11, MustMatch [[42, 31], [42, 11, 31]])  -- (42 | 31)⁺
               ]

-- | Get all messages that match the rules.
allMatches :: (RuleMap, [Text]) -> [Text]
allMatches (rules, messages) = filter (msgMatches rules) messages

type OtherRules = [Int]

data Rule
    = Single !Char
    | MustMatch ![OtherRules]

type RuleMap = HashMap Int Rule

-- | Does the given message match the rules?
msgMatches :: RuleMap -> Text -> Bool
msgMatches hm s = case hm !? 0 of
    Nothing  -> False
    Just rls -> go s [rls]
  where
    go str []     = T.null str
    go str (r:rs) = case r of
        Single    c    ->
            maybe False
                  (bool False (go (T.drop 1 str) rs) . (c ==))
                  (textToChar str)
        MustMatch oths ->
            any (maybe False (go str . (<> rs)) . traverse (hm !?))
                oths

-- | Parse the whole input into some rules, as well as the input
-- messages.
pInput :: Text -> (RuleMap, [Text])
pInput (splitOn "\n\n" -> [rs, ms]) = bimap (fromList . parse) parse
    ( (pRule `A.sepBy1` A.endOfLine, rs)
    , (pMessages                   , ms)
    )
  where
    parse :: (Parser p, Text) -> p
    parse =
        either (\s -> error $ "Parse error on " <> s) id . uncurry A.parseOnly
pInput _ = error "ERROR: Invalid input"

pMessages :: Parser [Text]
pMessages = A.takeTill (== '\n') `A.sepBy1` A.endOfLine

pRule :: Parser (Int, Rule)
pRule =
    (,) <$> A.decimal <* ":" <* A.skipSpace
        <*> (   MustMatch <$> pOtherRules
            <|> Single    <$> ("\"" *> A.anyChar <* "\"")
            )
  where
    pOtherRules :: Parser [OtherRules]
    pOtherRules = A.decimal `A.sepBy1` " " `A.sepBy1` " | "
