(ns clojure-solutions.day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.walk :refer [walk]])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day15.txt")
       str/split-lines
       (map #(str-to-coll-base 10 %))
       (map-matrix (fn [i j el] {[i j] el}))
       (into {})))

(defn- neighbours
  "Get neighbours for an index that are _not_ in the given set and are in
  bounds."
  [[i j] cols rows seen]
  (filter (fn [[n m]] (and (not (contains? seen [n m]))
                           (< -1 n cols)
                           (< -1 m rows)))
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

(defn- widen [grid size dir]
  (letfn [(go [mult]
              (reduce (fn [acc [[r c] v]]
                        (assoc acc
                               (case dir
                                 :right [(+ r (* mult size)) c]
                                 :down [r (+ c (* mult size))])
                               (inc (rem (+ (dec mult) v) 9)))) ; lol
                      {}
                      grid))]
    (map go (range 0 5))))

(defn day15 []
  (let [grid (parse)
        dim  (inc (apply max (map ffirst grid)))
        dim5 (* 5 dim)
        big-grid (walk #(apply merge %) ; outer
                       #(apply merge %) ; inner
                       (map (fn [g] (widen g dim :right))
                            (widen grid dim :down)))
        costs (dijkstra [0 0]
                        (fn [p seen]
                          (map (fn [p*] [p* (big-grid p*)])
                               (neighbours p dim5 dim5 seen))))]
    (println (costs [(dec dim)  (dec dim)]))  ;; => 687
    (println (costs [(dec dim5) (dec dim5)])) ;; => 2957
    ))
