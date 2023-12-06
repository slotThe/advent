(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day01 :as day01]
            [clojure-solutions.day03 :as day03]
            [clojure-solutions.day04 :as day04]
            [clojure-solutions.day05 :as day05]
            [clojure-solutions.day06 :as day06])
  (:use [clojure-aoc-util.util] :reload))

(defn -main [& _]
  (print-day 1 (day01/day01 :one) (day01/day01 :two))
  (print-day 3 (day03/day03 :one) (day03/day03 :two))
  (print-day 4 (day04/day04 :one) (day04/day04 :two))
  (print-day 5 (day05/day05 :one) (day05/day05 :two))
  (print-day 6 (day06/day06 :one) (day06/day06 :two)))
