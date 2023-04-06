(ns clojure-solutions.day12
  (:require [clojure-solutions.coords :as coords]
            [clojure.string :as str]
            [clojure.set :refer [map-invert]])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (str/split-lines (slurp "../inputs/day12.txt")))

(defn- neighbours
  "Get neighbours for an index that are _not_ in the given set and are in
  bounds."
  [lookup [i j] rows cols seen]
  (filter (fn [[n m]]
            (not (or (contains? seen [n m])
                     (< 1 (- (get lookup [n m])
                             (get lookup [i j]))))))
          (coords/neighbours4 [i j] rows cols)))

(defn- char->int [c]
  (int (cond (= c \S) \a
             (= c \E) \z
             :else    c)))

(defn day12 [p]
  (let [land (coords/seq->map (parse))
        landscape (map-val char->int land)
        [x-dim y-dim] (coords/dimensions land)
        start-at (case p
                   :one \S
                   :two \a)
        goal (get (map-invert land) \E)]
    (letfn [(shortest-path [start]
              (dijkstra start
                        (fn [p seen]
                          (map (fn [n] [n 1])
                               (neighbours landscape p x-dim y-dim seen)))))]
      (->> land
           (filter-val (partial = start-at)) ; look for all starting positions
           keys                              ; get coordinates
           shortest-path                     ; compute map of shortest paths
           (#(get % goal))                   ; shortest path to goal for each starting pos
           ))))
