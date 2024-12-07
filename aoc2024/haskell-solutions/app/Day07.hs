{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DerivingStrategies #-}
import Util hiding (Op)
import qualified Data.Text as T
import qualified Data.Text.IO as T

sim :: Int -> [Int] -> [Int -> Int -> Int] -> Bool
sim goal xs ops =
  goal `elem` foldl' (\acc x -> [e | a <- acc, o <- ops, let e = a `o` x, e <= goal])
                [head xs]
                (tail xs)

main :: IO ()
main = do
  inp <- map (bimap tread (map tread . T.splitOn " " . T.drop 2) . T.breakOn ":")
      . T.lines <$> T.readFile "../inputs/day07.txt"
  print $ sum [ a | (a, b) <- inp, sim a b [(+), (*)] ]
  print $ sum [ a | (a, b) <- inp, sim a b [(+), (*), \a b -> read (show a <> show b)] ]
