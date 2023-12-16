(ns clojure-solutions.day16
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]
            [clojure-aoc-util.coords :as c]))

(defn- parse []
  (aoc/transpose (str/split-lines (slurp "../inputs/day16.txt"))))

(defn- simulate [grid start]
  (aoc/flood-fill
   start
   (fn [[beam dir] seen]
     (filter #(and (aoc/not-in? seen %)
                   (aoc/mat-ix grid (first %)))
             (case (aoc/mat-ix grid beam)
               \.  [[(c/move dir beam) dir]]
               \/  [(case dir
                      :E [(c/above beam) :N]
                      :N [(c/right beam) :E]
                      :S [(c/left  beam) :W]
                      :W [(c/below beam) :S])]
               \\  [(case dir
                      :E [(c/below beam) :S]
                      :S [(c/right beam) :E]
                      :N [(c/left  beam) :W]
                      :W [(c/above beam) :N])]
               \-  (if (contains? #{:N :S} dir)
                     [[(c/left beam) :W], [(c/right beam) :E]]
                     [[(c/move dir beam) dir]])
               \|  (if (contains? #{:E :W} dir)
                     [[(c/above beam) :N], [(c/below beam) :S]]
                     [[(c/move dir beam) dir]]))))))

(defn day16 [p]
  (let [grid (parse)
        xmax (count grid)
        ymax (count (first grid))
        starts (case p
                 :one [[[0 0] :E]]
                 :two (concat
                       (mapcat (fn [x] [[[x 0] :S] [[x (dec ymax)] :N]])
                               (range xmax))
                       (mapcat (fn [y] [[[0 y] :E] [[(dec xmax) y] :W]])
                               (range ymax))))]
    (->> starts
         (pmap #(into #{} (map first) (simulate grid %)))
         (map count)
         (apply max))))
