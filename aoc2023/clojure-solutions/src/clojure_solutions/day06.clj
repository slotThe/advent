(ns clojure-solutions.day06
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :refer [binary-search]]))

(defn- parse1 [inp]
  (let [[as bs] (map #(re-seq #"\d+" %) (str/split-lines inp))]
    (map (fn [a b] (map read-string [a b])) as bs)))

(defn- parse2 [inp]
  (->> (str/split-lines inp)
       (map (comp str/join (partial re-seq #"\d+")))
       (map read-string)))

(defn- solve
  "Note that ½t is the best time we can do. Hence, we merely need to look for
  the index below and above the middle where the curve dips below the (best)
  distance."
  [inp]
  (let [[time distance] inp
        mid (Math/divideExact time 2)]
    (letfn [(beats-best? [t]
              (> (* t (- time t))
                 distance))]
      (- (first (binary-search (comp not beats-best?) mid time)) ; first time we dip below
         (first (binary-search beats-best?            0   mid)))))) ; first time we dip above

(defn day06 [p]
  (let [inp (slurp "../inputs/day06.txt")]
    (case p
      :one (reduce * (map solve (parse1 inp)))
      :two (solve (parse2 inp)))))
