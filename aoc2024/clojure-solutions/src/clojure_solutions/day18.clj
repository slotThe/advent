(ns clojure-solutions.day18
  (:require [clojure-aoc-util.util :as util]
            [clojure-aoc-util.coords :as c])
  (:gen-class))

(defn- parse []
  (->> (slurp "../inputs/day18.txt")
       (re-seq #"\d+")
       (map read-string)
       (partition 2)
       (mapv vec)))

(defn- path-exists? [grid]
  (get (util/dijkstra
        [0 0]
        (fn [p seen]
          (for [n (c/neighbours4 p)
                :when (and (contains? grid n)
                           (not (contains? seen n)))]
            [n 1])))
       [70 70]))

(defn -main [& _args]
  (let [points (parse)
        grid (into #{} (for [x (range 0 71), y (range 0 71)] [x y]))
        one (path-exists? (reduce disj grid (take 1024 points)))
        _ (assert (= one 416))
        two (points (first
                     (util/binary-search
                      (fn [i] (not (path-exists? (reduce disj grid (take i points)))))
                      0
                      (count points))))
        _ (assert (= two [50 23]))]
    (util/print-day 18 one two)))
