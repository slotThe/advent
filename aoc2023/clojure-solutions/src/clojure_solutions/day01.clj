(ns clojure-solutions.day01
  (:require [clojure.string :as str])
  (:use [clojure-aoc-util.util] :reload))

(defn- solve1 [input]
  (->> input
       (map (partial re-seq #"[1-9]"))
       (map (fn [xs] (parse-long (str/join [(first xs) (last xs)]))))
       sum))

(def replacements
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine" ])

(defn- solve2 [input]
  (letfn [(to-digit [[d s]]
            (if (not (empty? d))
              (parse-long d)
              (+ 1 (.indexOf replacements s))))]
    (->> input
         (map (partial re-seq
                       (re-pattern (str/join ["[1-9]|(?=("
                                              (str/join "|" replacements)
                                              "))"]))))
         (map (fn [xs] (+ (* (to-digit (first xs))
                             10)
                          (to-digit (last xs)))))
         sum)))

(defn day01 [part]
  (let [input (str/split-lines (slurp "../inputs/day01.txt"))]
    (case part
      :one (solve1 input)
      :two (solve2 input))))
