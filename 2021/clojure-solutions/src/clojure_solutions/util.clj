(ns clojure-solutions.util)

(defn str-to-coll-base [b s]
  (map #(Character/digit % b) s))

(defn coll-to-base [b xs]
  (BigInteger. (apply str xs) b))

(defn sum [xs]
  (reduce + xs))
