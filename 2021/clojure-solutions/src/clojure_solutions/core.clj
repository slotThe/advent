(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day2 :as day2]
            [clojure-solutions.day3 :as day3]
            [clojure-solutions.day4 :as day4]
            [clojure-solutions.day5 :as day5]
            [clojure-solutions.day6 :as day6]
            [clojure-solutions.day7 :as day7]
            [clojure-solutions.day8 :as day8]
            [clojure-solutions.day9 :as day9]
            [clojure-solutions.day10 :as day10]
            [clojure-solutions.day11 :as day11]
            [clojure-solutions.day12 :as day12]
            [clojure-solutions.day13 :as day13]
            [clojure-solutions.day14 :as day14]
            ))

(defn -main [& args]
  (println "Day Two:")
  (println (day2/one))
  (println (day2/two))

  (println "Day Three:")
  (println (day3/one))
  (println (day3/two))

  (println "Day Four:")
  (day4/day4)

  (println "Day Five:")
  (day5/day5)

  (println "Day Six:")
  (day6/day6)

  (println "Day Seven:")
  (day7/day7)

  (println "Day Eight:")
  (day8/day8)

  (println "Day Nine:")
  (day9/day9)

  (println "Day Ten:")
  (day10/day10)

  (println "Day Eleven:")
  (day11/day11)

  (println "Day Twelve:")
  (day12/day12)

  (println "Day Thirteen:")
  (day13/day13)

  (println "Day Forteen:")
  (day14/day14)
  )
