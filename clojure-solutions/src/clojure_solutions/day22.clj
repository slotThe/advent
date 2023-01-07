(ns clojure-solutions.day22
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:use [clojure-solutions.util] :reload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defn- parse []
  (->> (slurp "../inputs/day22.txt")
       split-groups
       ((fn [[board moves]]
          {:points (into {}
                         (filter (fn [[_ el]] (not= \space el)))
                         (map-matrix (fn [i j el] [[j i] el])
                                     (str/split-lines board)))
           :moves (map #(match %
                          "L" :L
                          "R" :R
                          n (read-string n))
                       (re-seq #"\d+|L|R" moves))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D coordinates (for next year I'll write a library!)

(defn- add-coords ^longs [^longs [x1 y1] ^longs [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- sub-coords ^longs [^longs [x1 y1] ^longs [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn- get-start [points]
  (->> points
       (filter (fn [[[_ j] _]] (= 0 j)))
       (apply min-key ffirst)
       first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving about

(defn- change-direction [dir looking]
  (let [dirs [[0 -1] [-1 0] [0 1] [1 0]]
        pos  (.indexOf dirs looking)]
    (case dir
      :L (nth dirs (mod (inc pos) 4))
      :R (nth dirs (mod (dec pos) 4)))))

(defn- calc-pos [points rounds pos dir]
  (loop [n rounds, p pos]
    (let [next (add-coords p dir)
          new-p (if (get points next)
                  next
                  (last (take-while
                         (partial contains? points)
                         (iterate #(sub-coords % dir) p))))]
      (if (or (= 0 n) (= \# (get points new-p)))
        p
        (recur (dec n) new-p)))))

(defn- move [{:keys [points moves]}]
  (reduce (fn [[pos dir] m]
            (match m
              (kw :guard keyword?) [pos (change-direction kw dir)]
              n                    [(calc-pos points n pos dir) dir]))
          [(get-start points) [1 0]]
          moves))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve

(defn- score [[[c r] d]]
  (+ (* 1000 (inc r))
     (* 4 (inc c))
     (match d
       [1  0] 0
       [0 -1] 1
       [-1 0] 2
       [0  1] 3)))

(defn day22 [p]
  (case p
    :one (score (move (parse)))))
