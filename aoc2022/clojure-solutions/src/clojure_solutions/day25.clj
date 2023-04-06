(ns clojure-solutions.day25
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- from-snafu [xs]
  (let [lookup {\2 2, \1 1, \0 0, \- -1, \= -2}]
    (reduce (fn [acc el]
              (+' (*' 5 acc) (get lookup el)))
            0
            xs)))

(defn- to-snafu [n]
  (letfn [(trans [n]
            (case n
              0 "0", 1 "1", 2 "2", 3 "=", 4 "-"))]
    (str/join
     (loop [num n, acc '()]
       (if (= num 0)
         acc
         (recur (Math/round (/ num 5.0))
                (conj acc (trans (mod num 5)))))))))

(defn day25 []
  (->> (slurp "../inputs/day25.txt")
       str/split-lines
       (map from-snafu)
       sum
       to-snafu))
