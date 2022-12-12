(ns clojure-solutions.day12
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]
            [clojure.data.priority-map :refer [priority-map]])
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

(defn- dijkstra
  "Dijkstra's shortest path algorithm.

  The given `more' function is a function of two arguments that computes
  the neighbours for each index.  It gets the index to compute the
  neighbours for, as well as the already seen points.  This allows for
  filtering while computing the neighbours, which may improve
  performance."
  [start more]
  (letfn [(mincost [cost cost*]
            (fn [cc] (if cc          ; if value exists, take the minimum
                       (min cc (+ cost cost*))
                       (+ cost cost*))))]
    (loop [all-costs  {}
           seen       #{}
           looking-at (priority-map start 0)]
      (if (empty? looking-at)        ; no more points to look at -> STOP
        all-costs
        (let [[u cost-u] (peek looking-at)
              tail (pop looking-at)]
          (if (seen u)               ;  check for seen in case `more' doesn't
            (recur all-costs seen tail)
            (recur (assoc all-costs u cost-u) ; final, minimal, cost of a point
                   (conj seen u)
                   (reduce (fn [acc [n cost-n]]
                             ;; Remember only the minimal cost of a neighbour.
                             (update acc n (mincost cost-u cost-n)))
                           tail
                           (more u seen)))))))))

(defn- get-starts [c landscape]
  (keys (filter (fn [[k v]] (= v c)) landscape)))

(defn- char->int [c]
  (int (cond (= c \S) \a
             (= c \E) \z
             true     c)))

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
