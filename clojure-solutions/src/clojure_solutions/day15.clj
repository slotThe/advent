(ns clojure-solutions.day15
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day15.txt")
       str/split-lines
       (map (comp (partial map read-string)
                  (partial re-seq #"-?\d+")))
       (map (fn [[s1 s2 b1 b2]]
              {:sensor [s1 s2]
               :beacon [b1 b2]}))))

(defn- manhattan ^long [^longs [x1 y1] ^longs [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn- no-beacons
  "The area in which no beacons can be.
  With `y' known, we need that

    ∥sensor - beacon∥ ≥ ∥sensor - (x, y)∥,

  which is equivalent to

    ∥sensor - beacon∥ - |sy - y| ≥ |sx - x|.

  When the lhs is positive, we have a non-zero number of places that a
  beacon can't be and return the respective x coordinates."
  [^long y {^longs [sx sy] :sensor, ^longs [bx by] :beacon}]
  (let [d (- (manhattan [sx sy] [bx by])
             (abs (- sy y)))]
    (when (>= d 0)
      [(- sx d) (+ sx d)])))

(defn- kill-overlapping [boxes]
  "Remove all overlapping boxes."
  (reduce (fn [[[ax ay] & as] [bx by]]
            (cond
              (nil? ax)        [[bx by]]
              (<= bx (inc ay)) (conj as [ax (max ay by)])
              :otherwise       (conj as [ax ay] [bx by])))
          []
          (sort boxes)))

(defn day15 [p]
  (let [inp (parse), y1 2000000, ymax 4000000]
    (letfn [(mk-box [y]
              (kill-overlapping (keep (partial no-beacons y) inp)))
            (tuning-frequency [[y [_ x]]]
              (+ (* (inc x) ymax) y))]
      (case p
        :one (- (sum (map (fn [[x1 x2]] (inc (- x2 x1)))
                          (mk-box y1)))
                (count (into #{}
                             (comp (map :beacon) (filter #(= (second %) y1)))
                             inp)))
        :two (tuning-frequency
              (first
               (for [y (range 0 (inc ymax))
                     :let [xs (first
                               (map (fn [[a b]] (when (> a 0) [0 a]))
                                    (mk-box y)))]
                     :when (some? xs)]
                 [y xs])))))))
