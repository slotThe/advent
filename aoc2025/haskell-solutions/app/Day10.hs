{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
import Util

import Data.Text qualified as T
import Data.Text (Text)
import Data.SBV

data Machine = Machine { goal :: !Int, buttons :: ![[Int]], jolts :: ![Integer] }

parse :: Text -> Machine
parse (T.splitOn " " -> xs) = Machine
  { goal = foldr1 (\x a -> 2*a + x) . map (\case '.' -> 0; '#' -> 1) . T.unpack $ T.filter (`elem` ['.','#']) l
  , buttons = map (rep "(" ")") bs
  , jolts = rep "{" "}" j
  }
 where
  ([l], (bs, [j])) = second (splitAt (length xs - 2)) $ splitAt 1 xs

  rep :: Read a => Text -> Text -> Text -> a
  rep o c = tread . T.replace c "]" . T.replace o "["

sim :: Machine -> Int
sim Machine{goal,buttons}
  = minimum . map length . filter (\xs -> goal == foldl' xor 0 xs)
  . subsequences . map (foldl' (\a i -> a .|. bit i) 0)
  $ buttons

solve2 :: Machine -> IO Int
solve2 Machine{buttons,jolts} = Util.fromEnum @Int32 . fromJust . getModelValue "p2" <$>
  optLexicographic do
    bps <- traverse free (map (("x" <>) . show) [0..length buttons-1]) -- button presses
    traverse_ (constrain . (.>= 0)) bps
    for_ (zip [0..] jolts) \(i, j) -> -- jolt should correspond to sum
      constrain (literal j .== sum [ p | (p, b) <- zip bps buttons, i `elem` b])
    minimize "min" (sum bps)

main :: IO ()
main = do
  inp <- map (parse . T.pack) . lines <$> readFile' "../inputs/day10.txt"
  t <- traverse solve2 inp
  day 10 $ pure $ both sum (map sim inp, t)
