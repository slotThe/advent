(ns clojure-solutions.day02
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :refer [sum]]))

(defn- possible [[n s]]
  (case s
    "red"   (<= n 12)
    "green" (<= n 13)
    "blue"  (<= n 14)))

(defn- parse []
  (->> (slurp "../inputs/day02.txt")
       str/split-lines
       (map (partial re-seq #"(\d+) (blue|red|green)"))
       (map (partial map (fn [[_ n s]] [(read-string n) s])))))

(defn day02 [p]
  (case p
    :one (->> (parse)
              (map (fn [n x] [x (inc n)]) (range))
              (keep (fn [[x n]] (when (every? possible x) n)))
              sum)
    :two (letfn [(power [game]
                   (->> game
                        (sort-by second)
                        (partition-by second)
                        (map (fn [xs] (first (apply max-key first xs))))
                        (reduce *)))]
           (sum (map power (parse))))))
