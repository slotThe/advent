(ns clojure-solutions.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]])
  (:use [clojure-solutions.util] :reload))

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

(defn- find-bounds [hs]
  (letfn [(go [op]
              (reduce (fn [[lx ly lz] [x y z]]
                        [(op lx x) (op ly y) (op lz z)])
                      hs))]
    [(map dec (go min)) (map inc (go max))]))

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
        [l h] (find-bounds rocks)]
    (case p
      :one (count
            (mapcat (partial neighbours #(not (contains? rocks %)))
                    rocks))
      :two (->> (dijkstra l (fn [p seen]
                              (map (fn [n] [n 1])
                                   (air-neighbours rocks seen l h p))))
                keys
                (into #{})
                (mapcat (partial neighbours #(contains? rocks %)))
                count))))
