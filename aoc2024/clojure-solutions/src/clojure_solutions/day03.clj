(ns clojure-solutions.day03
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util])
  (:gen-class))

(defn- parse1 []
  (->> (slurp "../inputs/day03.txt")
       str/split-lines
       (map #(->> %
                  (re-seq #"mul\((\d+),(\d+)\)")
                  (map (fn [[_ x y]] (* (parse-long x) (parse-long y))))
                  (reduce +)))
       (reduce +)))

(defn- parse2 []
  (->> (slurp "../inputs/day03.txt") ; No split lines here!
       (re-seq #"(mul\((\d+),(\d+)\))|do\(\)|don't\(\)")
       (map (fn [[a _ x y]]
              (case a
                "don't()" :dont
                "do()" :do
                (* (parse-long x) (parse-long y)))))
       (reduce (fn [[e acc] x]
                 (case x
                   :dont [false acc]
                   :do [true acc]
                   [e (+ acc (if e x 0))]))
               [true 0])
       second))

(defn -main [& _args]
  (util/print-day 3 (parse1) (parse2)))
