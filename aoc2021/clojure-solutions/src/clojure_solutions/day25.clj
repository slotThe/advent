(ns clojure-solutions.day25
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day25.txt")
       str/split-lines
       (map-matrix (fn [i j el]
                     (case el
                       \> {[j i] :right}
                       \v {[j i] :down }
                       nil)))
       (into {})))

(defn- update-place [g {:keys [orig new dir]}]
  (assoc (dissoc g orig) new dir))

(defn- step [n m grid]
  (let [neighbour (fn [g [[i j] dir]]
                    (let [candidate (case dir
                                      :right [(mod (inc i) n) j]
                                      :down [i (mod (inc j) m)])]
                      (when-not (g candidate)
                        {:orig [i j]
                         :new candidate
                         :dir dir})))
        ;; east-facing
        updates1 (keep #(neighbour grid %)
                       (filter-val (partial = :right) grid))
        new-grid (reduce update-place grid updates1)
        ;; south-facing
        updates2 (keep #(neighbour new-grid %)
                       (filter-val (partial = :down) new-grid))
        final-grid (reduce update-place new-grid updates2)]
    final-grid))

(defn day25 []
  (let [grid (parse)
        [n m] (map inc (reduce (fn [[x-max y-max] [x y]]
                                 [(max x-max x) (max y-max y)])
                               (keys grid)))]
    ;; => 598
    (println (first (converge-when (partial step n m) grid)))))
