(ns clojure-solutions.day15
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util]
            [clojure-aoc-util.coords :as c])
  (:gen-class))

(defn- parse [widen]
  (let [[b ms] (util/split-groups (slurp "../inputs/day15.txt"))]
    {:board (->> (str/split-lines b)
                 (widen)
                 (util/map-over-matrix (fn [i j el] [[j i] el]))
                 (apply concat)
                 (into {}))
     :moves (map #(case % \^ c/north, \> c/east, \< c/west, \v c/south)
                 (apply concat (str/split-lines ms)))}))

(defn- get-start [grid]
  (ffirst (filter (fn [[_ v]] (= v \@)) grid)))

(defn- sim [grid start dirs]
  (loop [pps [start], g grid, dds dirs, seen #{}]
    (cond
      (empty? pps) (let [ng (if-not seen
                              g
                              (let [ks (map #(vector % (get g %)) seen)]
                                (reduce (fn [acc [p v]] (assoc acc (c/add p (first dds)) v))
                                        (reduce (fn [acc [p _]] (assoc acc p \.)) g ks)
                                        ks)))]
                     (recur [(get-start ng)] ng (rest dds) #{}))
      (empty? dds) g
      :else (let [p (peek pps), ps (pop pps), [d & _] dds, np (c/add p d)]
              (if (contains? seen p)
                (recur ps g dds seen)
                (case (get g np)
                  \# (recur [] g dds nil) ; A single wall kills the whole run
                  \. (recur ps                        g dds (conj seen p))
                  \O (recur (conj ps np)              g dds (conj seen p))
                  \[ (recur (conj ps np (c/right np)) g dds (conj seen p))
                  \] (recur (conj ps np (c/left np))  g dds (conj seen p))))))))

(defn- solve [input]
  (let [{:keys [board moves]} input]
    (->> (sim board (get-start board) moves)
         (keep (fn [[p v]] (when (contains? #{\O \[} v) p)))
         (map (fn [[x y]] (+ x (* y 100))))
         (reduce +))))

(defn- widen [xs]
  (map (partial mapcat #(case % \# "##", \O "[]", \. "..", \@ "@."))
       xs))

(defn -main [& _args]
  (let [one (solve (parse identity)), _ (assert (= one 1552463))
        two (solve (parse widen)), _ (assert (= two 1554058))]
    (util/print-day 15 one two)))
