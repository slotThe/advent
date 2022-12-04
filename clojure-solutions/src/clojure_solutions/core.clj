(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day1 :as day1]
            [clojure-solutions.day2 :as day2]
            [clojure-solutions.day3 :as day3]
            [clojure-solutions.day4 :as day4]))

(defn- print-day [day one two]
  (println "!!! Day" day "!!!")
  (println "First  Task:" one)
  (println "Second Task:" two)
  (println))

(defn -main [& args]
  (print-day 1 (day1/get-nth-most-wanted 1) (day1/get-nth-most-wanted 3))
  (print-day 2 (day2/day2 :one) (day2/day2 :two))
  (print-day 3 (day3/day3 :one) (day3/day3 :two))
  (print-day 4 (day4/day4 :one) (day4/day4 :two)))
