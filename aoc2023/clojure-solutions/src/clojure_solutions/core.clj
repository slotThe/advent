(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day01 :as day01]
            [clojure-solutions.day03 :as day03]
            [clojure-solutions.day04 :as day04]
            [clojure-solutions.day05 :as day05]
            [clojure-solutions.day06 :as day06]
            [clojure-solutions.day08 :as day08]
            [clojure-solutions.day09 :as day09]
            [clojure-solutions.day10 :as day10]
            [clojure-solutions.day11 :as day11]
            [clojure-solutions.day12 :as day12]
            [clojure-solutions.day13 :as day13]
            [clojure-solutions.day15 :as day15]
            [clojure-solutions.day16 :as day16]
            [clojure-solutions.day18 :as day18]
            [clojure-aoc-util.util :refer [print-day]]))

(defn -main [& _]
  (print-day 1  (day01/day01 :one) (day01/day01 :two))
  (print-day 3  (day03/day03 :one) (day03/day03 :two))
  (print-day 4  (day04/day04 :one) (day04/day04 :two))
  (print-day 5  (day05/day05 :one) (day05/day05 :two))
  (print-day 6  (day06/day06 :one) (day06/day06 :two))
  (print-day 8  (day08/day08 :one) (day08/day08 :two))
  (print-day 9  (day09/day09 :one) (day09/day09 :two))
  (print-day 10 (day10/day10 :one) (day10/day10 :two))
  (print-day 11 (day11/day11 :one) (day11/day11 :two))
  (print-day 12 (day12/day12 :one) (day12/day12 :two))
  (print-day 13 (day13/day13 :one) (day13/day13 :two))
  (print-day 15 (day15/day15 :one) (day15/day15 :two))
  (print-day 16 (day16/day16 :one) (day16/day16 :two))
  (print-day 18 (day18/day18 :one) (day18/day18 :two)))
