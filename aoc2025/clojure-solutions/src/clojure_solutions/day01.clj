(ns clojure-solutions.day01
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util])
  (:gen-class))

(defn- parse []
  (->> (slurp "../inputs/day01.txt")
       str/split-lines
       (map (fn [l] [(first l) (read-string (str/join (rest l)))]))))

(defn- one [xs]
  (count
   (filter #(= 0 %)
           (reductions (fn [d [dir n]]
                         (mod (+ d (case dir \L (- n), n))
                              100))
                       50
                       xs))))

(defn- two [xs]
  (first
   (reduce
    (fn [[z d] [dir n]]
      ;; Use symmetry to only handle the positive case.
      [(+ z (quot (+ n (case dir \R d, \L (mod (- d) 100))) 100))
       (mod (+ d (case dir \L (- n), n)) 100)])
    [0 50]
    xs)))

(defn -main [& _args]
  (let [inp (parse), f (one inp), s (two inp)]
    (assert (and (= 1011 f) (= 5937 s)))
    (util/print-day 1 f s)))
