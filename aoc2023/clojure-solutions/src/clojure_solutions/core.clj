(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day01 :as day01]
            [clojure-solutions.day03 :as day03]
            [clojure-solutions.day04 :as day04])
  (:use [clojure-aoc-util.util] :reload))

(defn -main [& _]
  (print-day 1 (day01/day01 :one) (day01/day01 :two))
  (print-day 3 (day03/day03 :one) (day03/day03 :two))
  (print-day 4 (day04/day04 :one) (day04/day04 :two)))
