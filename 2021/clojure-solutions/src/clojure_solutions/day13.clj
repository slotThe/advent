(ns clojure-solutions.day13
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (letfn [(parse-xy [a]
            ((fn [[xy _eq & n]]         ; "x=123" or "y=123"
               [xy (coll-to-base 10 n)])
             (drop-while #(and (not= % \x) (not= % \y))
                         a)))]
    (->> (slurp "./input/day13.txt")
         (#(str/split % #"\n\n"))       ; split points from folds
         (map str/split-lines)          ; split points and folds into singletons
         ((fn [[points folds]]
            {:folds (map parse-xy folds)
             :dots  (set (map #(read-string (str/join ["[" % "]"])) ; read as vector
                              points))})))))

(defn- solve
  "Apply all folds to a set of points."
  [folds dots]
  (letfn [(do-fold [[xy a] [x y]]
            (case xy                    ; fold either along x or y axis
              \x (if (> x a) [(- a (- x a)) y            ] [x y])
              \y (if (> y a) [x             (- a (- y a))] [x y])))]
    (reduce (fn [acc f]
              (into #{} (map #(do-fold f %) acc)))
            dots
            folds)))

(defn- plot-set
  "Plot a set of points of the form [x y] that are in the positive
  quadrant.  ASCII style."
  [s]
  (let [[xm ym] (reduce (fn [[x-max y-max] [x y]]
                          [(max x-max x) (max y-max y)])
                        s)]
    (map (fn [line] (str/join
                     (map (fn [[a b]] (if (s [b a]) "█" " "))
                          line)))
         (map (fn [a] (map #(vector a %)
                           (range 0 (inc xm))))
              (range 0 (inc ym))))))

(defn day13 []
  (let [{:keys [folds dots]} (parse)]
    (println (count (solve (take 1 folds) dots))) ; => 607
    (doseq [line (plot-set (solve folds dots))]   ; => CPZLPFZL
      (println line))))
