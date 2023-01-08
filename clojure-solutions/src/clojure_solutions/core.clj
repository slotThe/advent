(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day1 :as day1]
            [clojure-solutions.day2 :as day2]
            [clojure-solutions.day3 :as day3]
            [clojure-solutions.day4 :as day4]
            [clojure-solutions.day5 :as day5]
            [clojure-solutions.day6 :as day6]
            [clojure-solutions.day7 :as day7]
            [clojure-solutions.day9 :as day9]
            [clojure-solutions.day10 :as day10]
            [clojure-solutions.day12 :as day12]
            [clojure-solutions.day13 :as day13]
            [clojure-solutions.day14 :as day14]
            [clojure-solutions.day15 :as day15]
            [clojure-solutions.day17 :as day17]
            [clojure-solutions.day18 :as day18]
            [clojure-solutions.day19 :as day19]
            [clojure-solutions.day22 :as day22]
            [clojure-solutions.day23 :as day23]
            [clojure-solutions.day25 :as day25]))

(defn- print-day [day one two]
  (println "!!! Day" day "!!!")
  (println "First  Task:" one)
  (println "Second Task:" two)
  (println))

(defn -main [& args]
  (print-day 1 (day1/get-nth-most-wanted 1) (day1/get-nth-most-wanted 3))
  (print-day 2 (day2/day2 :one) (day2/day2 :two))
  (print-day 3 (day3/day3 :one) (day3/day3 :two))
  (print-day 4 (day4/day4 :one) (day4/day4 :two))
  (print-day 5 (day5/day5 :one) (day5/day5 :two))
  (print-day 6 (day6/day6 :one) (day6/day6 :two))
  (print-day 7 (day7/day7 :one) (day7/day7 :two))
  (print-day 9 (day9/day9 :one) (day9/day9 :two))
  (print-day 10 (day10/day10 :one) (day10/day10 :two))
  (print-day 12 (day12/day12 :one) (day12/day12 :two))
  (print-day 13 (day13/day13 :one) (day13/day13 :two))
  (print-day 14 (day14/day14 :one) (day14/day14 :two))
  (print-day 15 (day15/day15 :one) (day15/day15 :two))
  (print-day 17 (day17/day17 :one) (day17/day17 :two))
  (print-day 18 (day18/day18 :one) (day18/day18 :two))
  (print-day 19 (day19/day19 :one) (day19/day19 :two))
  (print-day 22 (day22/day22 :one) (day22/day22 :two))
  (print-day 23 (day23/day23 :one) (day23/day23 :two))
  (print-day 25 (day25/day25) "Merry Christmas!"))
