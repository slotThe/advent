(ns clojure-solutions.day18
  (:require [clojure.string :as str]
            [clojure-aoc-util.coords :as coords])
  (:use [clojure-aoc-util.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day18.txt")
       str/split-lines
       (map #(re-seq #"\d+" %))
       (into #{}
             (map #(mapv read-string %)))))

(defn- neighbours [f [x y z]]
  (filter f
          [[(inc x) y z] [x (inc y) z] [x y (inc z)]
           [(dec x) y z] [x (dec y) z] [x y (dec z)]]))

(defn- air-neighbours [rocks seen [lx ly lz] [hx hy hz] pt]
  (neighbours (fn [[n m k]]
                (and (not (contains? rocks [n m k]))
                     (not (contains? seen [n m k]))
                     (<= lx n hx)
                     (<= ly m hy)
                     (<= lz k hz)))
              pt))

(defn day18 [p]
  (let [rocks (parse)
        [l h] (coords/bounding-box rocks)]
    (count
     (case p
       :one (mapcat (partial neighbours #(not (contains? rocks %)))
                    rocks)
       :two (mapcat (partial neighbours #(contains? rocks %))
                    (flood-fill l #(air-neighbours rocks %2 l h %1)))))))
