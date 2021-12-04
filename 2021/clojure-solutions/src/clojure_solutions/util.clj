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

;;; Parsing

(defn words [s]
  (str/split s #" "))
