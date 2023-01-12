(ns clojure-solutions.day8
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day8.txt")
       str/split-lines
       (mapv (comp vec (partial str-to-coll-base 10)))))

(defn- solve [trans together xxs]
  (letfn [(go [xs]                 ; 'xs' represents a single row/column
              (map together
                   (trans xs)                        ; facing one way
                   (reverse (trans (reverse xs)))))] ; facing the other way
    (apply concat
           (map (partial map together)
                (map go xxs)                         ; horizontal
                (transpose (map go (transpose xxs))) ; vertical
                ))))

(defn- find-index [p xs]
  (loop [ys xs, i 0]
    (when-not (empty? ys)
      (if (p (first ys)) i (recur (rest ys) (inc i))))))

(defn day8 [p]
  (let [input (parse)]
    (letfn [(visibles [xs]
              (map > xs (reductions max Long/MIN_VALUE xs)))
            (score [xs]
              (for [[y & ys] (drop-last (tails xs))
                    :let [i (find-index #(>= % y) ys)]]
                (if i (inc i) (count ys))))]
      (case p
        :one (reduce #(if (true? %2) (inc %1) %1)
                     0
                     (solve visibles #(or %1 %2) input))
        :two (apply max (solve score * input))))))
