(ns clojure-solutions.day11
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (let [input (str/split-lines (slurp "./input/day11.txt"))
        grid (->> input
                  (map #(str-to-coll-base 10 %))
                  (map-matrix (fn [i j el] {[i j] el}))
                  (apply merge))]
    {:height (count input)
     :width  (count (first input))
     :grid   grid}))

(defn- neighbours-in-bounds
  "Get all in-bound neighbours of an index."
  [i j cols rows]
  (into {}
        (comp (filter (fn [[n m]] (and (< -1 n cols) (< -1 m rows))))
              (map    (fn [ix]    {ix 1})))
        [[(dec i) (dec j)] [(dec i) j      ] [i       (dec j)]
         [(inc i) j      ] [i       (inc j)] [(inc i) (inc j)]
         [(inc i) (dec j)] [(dec i) (inc j)]]))

(defn- excited? [x] (keyword? x))

(defn- step
  "A single evolution step."
  [neighbours grid]
  (->> grid
       (map-val inc)                    ; increase everything
       (converge
        #(->> %
              (map (fn [[[i j] v]]
                     (if (or (excited? v) (<= v 9))
                       ;; Identity if already excited or not high enough
                       {[i j] v}
                       ;; We just became excited, so inform neighbours
                       (merge {[i j] :excited} (neighbours i j)))))
              (apply merge-with (fn [a b]
                                  ;; Excited values swallow everything
                                  (if (or (excited? a) (excited? b))
                                    :excited
                                    (+ a b))))))))

(defn- solve [neighbours grid]
  (iterate (fn [[flashes xs]]
             (let [grid (step neighbours xs)]
               [(+ flashes (count (filter-val excited? grid)))
                (map-val #(if (excited? %) 0 %) grid)]))
           [0 grid]))

(defn day11 []
  (let [{:keys [grid width height]} (parse)
        max-keys (* width height)
        neighbours (fn [i j] (neighbours-in-bounds i j width height))
        list (solve neighbours grid)]
    ;; => 1739
    (println (first (nth list 100)))
    ;; => 324
    (println (count (take-while (fn [xs]
                                  (not= max-keys
                                        (count (filter-val #(= 0 %) xs))))
                                (map second list))))))
