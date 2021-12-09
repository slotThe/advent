(ns clojure-solutions.util
  (:require [clojure.string :as str]))

;;; Converting

(defn str-to-coll-base [b s]
  (map #(Character/digit % b) s))

(defn coll-to-base [b xs]
  (BigInteger. (apply str xs) b))

;;; Stuff that should be in clojure.core

(defn sum [xs]
  (reduce + xs))

(defn transpose [mat]
  (apply mapv vector mat))

(defn elem [x xs]
  (some #{x} xs))

(defn permutations [[h & t :as coll]]
  (if (nil? t)
    [coll]
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

;;; Matrix manipulation

(defn- mat-ix
  "Return the element at index (i, j).  Returns `nil' if index is
  out-of-bounds."
  [m [i j]]
  (let [rows (count m)
        cols (count (first m))]
    (when (and (< -1 i rows) (< -1 j cols))
      (nth (nth m i) j))))

(defn map-matrix
  "Map a function f(i, j, el) over all element of a matrix with
  indices."
  [mat f]
  (apply concat
         (keep-indexed (fn [i row]
                         (keep-indexed (fn [j el]
                                         (f i j el))
                                       row))
                       mat)))

;;; Parsing

(defn words [s]
  (str/split s #" "))
