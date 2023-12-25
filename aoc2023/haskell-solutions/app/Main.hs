import Control.Parallel.Strategies (parMap, rseq)
import Day01
import Day02
import Day07
import Day09
import Day16
import Day19
import Day22
import Day25
import Util

main :: IO ()
main = sequenceA_ . parMap rseq (uncurry day) $
  [ (1,  day01)
  , (2,  day02)
  , (7,  day07)
  , (9,  day09)
  , (16, day16)
  , (19, day19)
  , (22, day22)
  , (25, day25)
  ]
