(ns clojure-solutions.day12
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (str/split-lines (slurp "../inputs/day12.txt")))

(defn- neighbours
  "Get neighbours for an index that are _not_ in the given set and are in
  bounds."
  [lookup [i j] cols rows seen]
  (filter (fn [[n m]] (and (not (contains? seen [n m]))
                           (< -1 n cols)
                           (< -1 m rows)
                           (not (< 1 (- (get lookup [n m])
                                        (get lookup [i j]))))))
          [[i (inc j)] [(inc i) j]
           [(dec i) j] [i (dec j)]]))

(defn- get-starts [c landscape]
  (keys (filter (fn [[_ v]] (= v c)) landscape)))

(defn- char->int [c]
  (int (cond (= c \S) \a
             (= c \E) \z
             :else    c)))

(defn day12 [p]
  (let [grid (parse)
        [c r] [(count grid) (count (first grid))]
        land (into {} (map-matrix (fn [i j el] {[i j] el}) grid))
        e (get (map-invert land) \E)
        landscape (map-val char->int land)]
    (letfn [(solve [start]
              (dijkstra start
                        (fn [p seen]
                          (map (fn [n] [n 1])
                               (neighbours landscape p c r seen)))))]
      (apply min
             (keep #(get % e)
                   (map solve (get-starts (case p
                                            :one \S
                                            :two \a)
                                          land)))))))
