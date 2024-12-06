(ns clojure-solutions.day06
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util]
            [clojure-aoc-util.coords :as c])
  (:gen-class))

(defn- parse []
  ;; For some reason util/coords is the wrong way around!
  (util/transpose (mapv vec (str/split-lines (slurp "../inputs/day06.txt")))))

(defn- find-start [grid]
  (first (for [i (range 0 (count grid)), j (range 0 (count (nth grid i)))
               :when (= (util/mat-ix grid [i j]) \^)]
           [i j])))

(defn- sim [grid start]
  (loop [pos start, dir c/north, visited #{[start dir]}]
    (let [new-pos (c/add pos dir)]
      (if (contains? visited [new-pos dir])
        {:loop true}
        (case (util/mat-ix grid new-pos)
          nil visited                            ; Falling off
          \# (recur pos (c/turn :R dir) visited) ; Banging head
          (recur new-pos dir (conj visited [new-pos dir])))))))

(defn -main [& _args]
  (let [grid (parse)
        start (find-start grid)
        path (into #{} (map first (sim grid start)))]
    (util/print-day
     6
     (count path)
     (->> (disj path start)
          (map #(sim (assoc-in grid % \#) start))
          (filter :loop)
          count))))
