{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

import Util
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.MemoTrie

arrangements :: [Text] -> Text -> Int
arrangements towels = memoFix go
 where
  go :: (Text -> Int) -> Text -> Int
  go go ptn
    | T.null ptn = 1
    | otherwise  = sum [ go (T.drop (T.length t) ptn) | t <- towels , t `T.isPrefixOf` ptn ]

main :: IO ()
main = do
  (towels, ptns) <- bimap (T.splitOn ", ") (T.lines . T.drop 2) . T.breakOn "\n\n"
               <$> T.readFile "../inputs/day19.txt"
  let arrs = map (arrangements towels) ptns
  day 19 $ pure (length $ filter (> 0) arrs, sum arrs)

instance HasTrie Text where
  newtype Text :->: a = TextTrie (Either () (Char, Text) :->: a)

  trie :: (Text -> a) -> Text :->: a
  trie f = TextTrie (trie (f . text))

  untrie :: (Text :->: a) -> Text -> a
  untrie (TextTrie t) = untrie t . detext

  enumerate :: (Text :->: a) -> [(Text, a)]
  enumerate (TextTrie t) = (fmap . first) text (enumerate t)

text :: Either () (Char, Text) -> Text
text = either (const "") (uncurry T.cons)

detext :: Text -> Either () (Char, Text)
detext t = case T.uncons t of
  Nothing      -> Left ()
  Just (x, xs) -> Right (x, xs)
