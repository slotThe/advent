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

(defn- path-exists? [grid & shortest]
  (let [dijkstra? (:shortest (set shortest))]
    (get ((if dijkstra? util/dijkstra util/flood-fill)
          [0 0]
          (fn [p seen]
            (for [n (c/neighbours4 p)
                  :when (and (contains? grid n)
                             (not (contains? seen n)))]
              (if dijkstra? [n 1] n))))
         [70 70])))

(defn -main [& _args]
  (let [points (parse)
        grid (reduce disj
                     (into #{} (for [x (range 0 71), y (range 0 71)] [x y]))
                     (take 1024 points))
        one (path-exists? grid :shortest)
        _ (assert (= one 416))
        two (reduce (fn [acc p]
                      (let [grid (disj acc p)]
                        (if (path-exists? grid) grid (reduced p))))
                    grid
                    (drop 1024 points))
        _ (assert (= two [50 23]))]
    (util/print-day 18 one two)))
