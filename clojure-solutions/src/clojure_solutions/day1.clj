(ns clojure-solutions.day1
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day1.txt")
       split-groups
       (map str/split-lines)
       (map #(map read-string %))
       (map sum)))

(defn get-nth-most-wanted [n]
  (sum (take n (sort > (parse)))))
