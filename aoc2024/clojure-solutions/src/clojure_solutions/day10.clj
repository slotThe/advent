(ns clojure-solutions.day10
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util]
            [clojure-aoc-util.coords :as c])
  (:gen-class))

(defn- parse []
  (->> (slurp "../inputs/day10.txt")
       str/split-lines
       (util/map-over-matrix (fn [i j c] [[j i] (read-string (str c))]))
       (apply concat)
       (into {})))

(defn- trailheads [grid start & {:keys [routes]}]
  (loop [queue [start], heads 0, seen #{start}]
    (if (empty? queue)
      heads
      (let [[[hp k] ts] [(peek queue) (pop queue)]
            ns (for [np (c/neighbours4 hp)
                     :let [nk (get grid np nil)]
                     :when (and (= nk (inc k))
                                (or routes (not (contains? seen [np nk]))))]
                 [np nk])]
        (recur (apply conj ts ns)
               (+ heads (count (filter (fn [[_ nk]] (= nk 9)) ns)))
               (apply conj seen ns))))))

(defn- solve [grid starts & {:keys [routes]}]
  (reduce + (map #(trailheads grid % :routes routes) starts)))

(defn -main [& _args]
  (time (let [grid (parse)
              starts (filter (fn [[_ v]] (= v 0)) grid)
              one (solve grid starts)
              two (solve grid starts :routes true)]
          (assert (= one 629)) (assert (= two 1242))
          (util/print-day 10 one two))))
