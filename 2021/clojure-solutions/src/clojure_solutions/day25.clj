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

(defn- step [grid]
  (let [[n m] (reduce (fn [[x-max y-max] [x y]]
                        [(max x-max x) (max y-max y)])
                      (keys grid))]
    (letfn [(neighbour [g [[i j] dir]]
              (let [candidate (case dir
                                :right [(mod (inc i) (inc n)) j]
                                :down [i (mod (inc j) (inc m))])]
                (when-not (g candidate)
                  {:orig [i j]
                   :new candidate
                   :dir dir})))
            (update-place [g {:keys [orig new dir]}]
              (assoc (dissoc g orig) new dir))]
      (let [updates1 (keep #(neighbour grid %)
                           (filter-val (partial = :right) grid))
            new-grid (reduce update-place grid updates1)
            updates2 (keep #(neighbour new-grid %)
                           (filter-val (partial = :down) new-grid))]
        (reduce update-place new-grid updates2)))))

(defn day25 []
  (println (first (converge-when step (parse)))))
