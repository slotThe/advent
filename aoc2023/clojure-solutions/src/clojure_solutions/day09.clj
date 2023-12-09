(ns clojure-solutions.day09
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]))

(defn- parse []
  (->> (slurp "../inputs/day09.txt")
       str/split-lines
       (mapv (comp (partial mapv read-string) aoc/words))))

(defn- mk-sequences [xs]
  (loop [[r & _ :as res] (list xs)]
    (if (every? zero? r)
      res
      (recur (conj res (mapv (fn [a b] (- b a)) r (rest r)))))))

(defn day09 [p]
  (let [readings (parse)]
    (letfn [(solve [extract gen-val]
              (aoc/sum (map #(extract (reduce gen-val (mk-sequences %)))
                            readings)))]
      (case p
        :one (solve peek  (fn [xs ys]
                            (let [x (peek xs), y (peek ys)]
                              (conj ys (+ y x)))))
        :two (solve first (fn [[x :as _] [y :as ys]]
                            (cons (- y x) ys)))))))
