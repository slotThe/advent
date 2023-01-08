(ns clojure-solutions.day2
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- game-result [op me]
  (case [op me]
    [:A :X] 3
    [:A :Y] 6
    [:A :Z] 0
    [:B :X] 0
    [:B :Y] 3
    [:B :Z] 6
    [:C :X] 6
    [:C :Y] 0
    [:C :Z] 3))

(defn- weapons-worth [weapon]
  (case weapon
    :X 1
    :Y 2
    :Z 3))

(defn- transform [op me]
  (let [outcome (case me
                  :X 0
                  :Y 3
                  :Z 6)]
    (first (for [cand [:X :Y :Z]
                 :when (= outcome (game-result op cand))]
             cand))))

(defn- solve [get-weapon]
  (->> (slurp "../inputs/day2.txt")
       str/split-lines
       (map #(map keyword (str/split % #" ")))
       (map (fn [[op me]]
              (let [my-weapon (get-weapon op me)]
                (+ (game-result op my-weapon)
                   (weapons-worth my-weapon)))))
       sum))

(defn day2 [kw]
  (case kw
    :one (solve (fn [_ b] b))
    :two (solve transform)))
