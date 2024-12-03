(ns clojure-solutions.day03
  (:require [clojure-aoc-util.util :as util])
  (:gen-class))

(defn- parse1 []
  (->> (slurp "../inputs/day03.txt")
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ x y]] (* (parse-long x) (parse-long y))))
       (reduce +)))

(defn- parse2 []
  (->> (slurp "../inputs/day03.txt") ; No split lines here!
       (re-seq #"(mul\((\d+),(\d+)\))|do\(\)|don't\(\)")
       (reduce (fn [[e acc] [a _ x y]]
                 (case a
                   "don't()" [false acc]
                   "do()" [true acc]
                   [e (+ acc (if e (* (parse-long x) (parse-long y)) 0))]))
               [true 0])
       second))

(defn -main [& _args]
  (util/print-day 3 (parse1) (parse2)))
