(ns clojure-solutions.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure-aoc-util.util :refer [sum]]))

(defn- parse []
  (into {}
        (comp (map #(str/split % #"\|"))
              (map (partial map #(map read-string (re-seq #"\d+" %))))
              (map (fn [[have winning]]
                     {(nth have 0)              ; card number
                      [1                        ; multiplicity
                       (count (set/intersection ; number of winning guesses
                               (into #{} (drop 1 have))
                               (into #{} winning)))]})))
        (str/split-lines (slurp "../inputs/day04.txt"))))

(defn- calc-points [[_ [_ correct-guesses]]]
  (int (Math/pow 2 (dec correct-guesses))))

(defn- part2 [hm]
  (let [max (count hm)]
    (map (fn [[_ [mult _]]] mult)
         (loop [cards hm, n 1]
           (if (> n max)
             cards
             (let [[mult correct-guesses] (get cards n)
                   wins (take correct-guesses (iterate inc (inc n)))]
               (recur (reduce #(update %1 %2 (fn [[n cg]] [(+ mult n) cg]))
                              cards
                              wins)
                      (inc n))))))))

(defn day04 [p]
  (let [cards (parse)]
    (sum (case p
           :one (map calc-points cards)
           :two (part2 cards)))))
