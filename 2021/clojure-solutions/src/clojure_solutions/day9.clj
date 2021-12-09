(ns clojure-solutions.day9
  (:require [clojure.string :as str]
            [clojure.set    :as set])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day9.txt")
       str/split-lines
       (map #(str-to-coll-base 10 %))))

(defn- neighbours
  "Get all horizontal/vertical neighbours of an index."
  [i j]
  [[(dec i) j] [i (dec j)]
   [(inc i) j] [i (inc j)]])

(defn- get-sinks [mat]
  (map-matrix mat
              (fn [i j el]
                (when (every? #(< el %)             ; is a sink?
                              (keep #(mat-ix mat %) ; get values of neighbours
                                    (neighbours i j)))
                  [el [i j]]))))    ; return sink value and indices

;; => 631
(defn- part1 [mat]
  (reduce (fn [acc [el _]] (+ acc (inc el)))
          0
          (get-sinks mat)))

(defn- basin-for
  "For a given matrix `mat', find the basin associated to `el', which
  should be a sink."
  [mat el]
  (letfn
      ((go [seen [n [i j]]]
           (let [bigger-nums       ; numbers bigger than the current one
                 (keep (fn [ixs]
                         (let [k (mat-ix mat ixs)]
                           (when (and (some? k) (> k n) (not= k 9))
                             [k ixs])))
                       (neighbours i j))]
             (if (empty? bigger-nums)
               (conj seen [i j])        ; no new numbers -> abort
               (apply set/union         ; new numbers    -> recurse
                      (map #(go (conj seen [i j]) %)
                           bigger-nums))))))
      (go #{} el)))

;; => 821560
(defn- part2 [mat]
  (->> (get-sinks mat)
       (map #(count (basin-for mat %)))
       (sort >)
       (take 3)
       (reduce *)))

(defn day9 []
  (let [mat (parse)]
    (println (part1 mat))
    (println (part2 mat))))
