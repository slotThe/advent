(ns clojure-solutions.day01
  (:require [clojure.string :as str])
  (:use [clojure-aoc-util.util] :reload))

(defn- get-last-two [twist xs]
  (parse-long (str/join [(twist (first xs)) (twist (last xs))])))

(defn- solve1 [input]
  (->> input
       (map #(re-seq #"[1-9]" %))
       (map (partial get-last-two identity))
       sum))

(defn- solve2 [input]
  (letfn [(to-digit [[d s]]
            (if (not (empty? d))
              d
              (case s
                "one"   "1"
                "two"   "2"
                "three" "3"
                "four"  "4"
                "five"  "5"
                "six"   "6"
                "seven" "7"
                "eight" "8"
                "nine"  "9"
                s)))]
    (->> input
         (map #(re-seq #"[1-9]|(?=(one|two|three|four|five|six|seven|eight|nine))" %))
         (map (partial get-last-two to-digit))
         sum)))

(defn day01 [part]
  (let [input (str/split-lines (slurp "../inputs/day01.txt"))]
    (case part
      :one (solve1 input)
      :two (solve2 input))))
