(ns clojure-solutions.day11
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]
            [clojure-aoc-util.coords :as c]))

(defn- parse [num]
  (letfn [(expand [expand-single grid]
            (loop [[g i] [grid 0]]
              (if (>= i (count g))
                g
                (recur (if (every? (fn [[_ s]] (= s \.)) (nth g i))
                         [(concat (take i g)
                                  (map (partial map (fn [[xy s]] [(expand-single xy) s]))
                                       (drop i g)))
                          (+ i 2)]
                         [g (+ i 1)])))))]
    (->> (slurp "../inputs/day11.txt")
         str/split-lines
         (aoc/map-over-matrix (fn [i j el] [[i j] el]))
         ;; Horizontal expansion.
         (expand (fn [[x y]] [(+' x (dec num)) y]))
         ;; Vertical expansion.
         aoc/transpose
         (expand (fn [[x y]] [x (+' y (dec num))]))
         aoc/transpose
         ;; Flatten the input.
         (apply concat))))

(defn- solve [num]
  (let [galaxies (keep (fn [[xy s]] (when (= s \#) xy)) (parse num))]
    (aoc/sum
     (mapcat (fn [p ps] (map (partial c/taxicab p) ps))
             galaxies
             (drop 1 (aoc/tails galaxies))))))

(defn day11 [p]
  (case p
    :one (solve 2)
    :two (solve 1000000)))
