(ns clojure-solutions.day08
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util]
            [clojure-aoc-util.coords :as c])
  (:gen-class))

(defn- parse []
  (->> (slurp "../inputs/day08.txt")
       str/split-lines
       ;; util/coords is still the wrong way around!
       (util/map-over-matrix (fn [i j c] [[j i] c]))
       (apply concat)
       (into {})))

(defn- solve [grid get-points]
  (letfn [(points-for-antenna [a]
            (map first (filter (fn [[_ x]] (= x a)) grid)))]
    (->> (for [ant (disj (into #{} (vals grid)) \.)
               p (points-for-antenna ant)
               np (points-for-antenna ant)
               :when (not= p np)]       ; heh
           (get-points grid p np))
         (apply concat)
         (into #{} (keep identity))
         count)))

(defn -main [& _args]
  (let [grid (parse)]
    (util/print-day
     8
     (solve grid                        ; 299
            (fn [grid p np]
              (let [ant (c/sub p (c/mulc 2 (c/sub p np)))]
                [(and (get grid ant nil) [ant])])))
     (solve grid                        ; 1032
            (fn [grid p np]
              (take-while (partial contains? grid)
                          (map #(c/sub p (c/mulc % (c/sub p np)))
                               (iterate inc 1))))))))
