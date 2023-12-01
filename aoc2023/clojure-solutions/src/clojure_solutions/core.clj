(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day01 :as day01])
  (:use [clojure-aoc-util.util] :reload))

(defn -main [& _]
  (print-day 1 (day01/day01 :one) (day01/day01 :two)))
