(ns clojure-solutions.day5
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day5.txt")
       str/split-lines
       (map #(read-string
              (str "[" (str/replace % #",| -> " " ") "]")))))

(defn- decide-range [x y]
  (if (<= x y)
    (range x (inc y))
    (range x (dec y) -1)))

(defn- solve [maps]
  (->> maps
       (mapcat
        (fn [[x1 y1 x2 y2]]
          (cond (= x1 x2) (map #(vector x1 %) (decide-range y1 y2))
                (= y1 y2) (map #(vector % y1) (decide-range x1 x2))
                :else (map #(vector %1 %2)
                           (decide-range x1 x2)
                           (decide-range y1 y2)))))
       frequencies
       (filter #(>= (val %) 2))
       count))

(defn day5 []
  (let [points (parse)]
    ;; part 1 => 6267
    (println (->> points
                  (filter (fn [[x1 y1 x2 y2]] ; only horiz/vert lines
                            (or (= x1 x2) (= y1 y2))))
                  solve))
    ;; part 2 => 20196
    (println (solve points))))
