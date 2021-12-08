(ns clojure-solutions.day8
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(def normal-segments
  "All number with their associated `regular' segments."
  (into {}
        (map (fn [[k v]] [(set k) v]))
        {"abcefg" 0, "cf" 1, "acdeg" 2, "acdfg" 3, "bcdf" 4, "abdfg" 5,
         "abdefg" 6, "acf" 7, "abcdefg" 8, "abcdfg" 9}))

(defn- parse []
  (letfn ((parse-line [line]
            (->> line
                 (#(str/split % #" \| "))
                 (map words))))
    (->> (slurp "./input/day8.txt")
         str/split-lines
         (map parse-line))))

(defn- part1
  "Filter for the lengths of the unique numbers."
  []
  ;; => 237
  (letfn ((filter-line [[signals output]]
            (filter #(#{2 3 4 7} (count %)) output)))
    (count (mapcat filter-line (parse)))))

(defn- get-combinations
  "Given some collection, create maps for all possible ways of permuting
  that collection."
  [coll]
  (map (fn [combs]
         (apply merge (map hash-map coll combs)))
       (permutations coll)))

(defn- part2 []
  ;; => 1009098N
  (let [combs (get-combinations "abcdefg")
        normal-keys (keys normal-segments)
        deduce-line (fn [[segments counts]]
                      (first
                       (for [comb combs            ; combination to look at
                             :let [replace-letters ; look up word with new wiring
                                   (fn [word]
                                     (set (map comb word)))]
                             :when                 ; if everything matches, we have a winner
                             (every? #(elem (replace-letters %) normal-keys)
                                     segments)]
                         (->> counts
                              (map replace-letters) ; replace with winning permutation
                              (map normal-segments) ; get numbers
                              (coll-to-base 10)))))]
    (sum (map deduce-line (parse)))))

(defn day8 []
  (println (part1))
  (println (part2)))
