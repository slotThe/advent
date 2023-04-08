(ns clojure-solutions.day23
  (:require [clojure.string :as str]
            [clojure-aoc-util.coords :as coords])
  (:use [clojure-aoc-util.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day23.txt")
       str/split-lines
       (coords/seq->map (filter-val (partial = \#)))
       keys
       (into #{})))

(defn- neighbours [f pt]
  (filter f
          (into {} (map vector
                        [:NW :N :NE :W :E :SW :S :SE]
                        (coords/neighbours8 pt)))))

(defn- propose-spot [neighs pt dir]
  (let [s (case dir
            :N #{:N :NE :NW}
            :S #{:S :SE :SW}
            :W #{:W :NW :SW}
            :E #{:E :NE :SE})]
    (when (not-any? s neighs)
      (coords/move dir pt))))

(defn- propose-move [scan dirs [x y]]
  (let [neighs (keys (neighbours (fn [[_ v]] (contains? scan v))
                                 [x y]))
        move-to (first (keep (partial propose-spot neighs [x y]) dirs))]
    [[x y] (cond (nil? neighs)   [x y]
                 (some? move-to) move-to
                 :else           [x y])]))

(defn- round [[scan dirs]]
  (letfn [(move [hs [to from]]
            (if (= 1 (count from))
              (conj! (disj! hs (ffirst from)) to)
              hs))]
    [(->> scan
          (map (partial propose-move scan dirs))
          (group-by second)
          (reduce move (transient scan))
          persistent!)
     (concat (rest dirs) [(first dirs)])]))

(defn day23 [p]
  (let [input (parse)
        dirs [:N :S :W :E]
        round10 (first (nth (iterate round [input dirs]) 10))
        [[lx ly] [hx hy]] (coords/bounding-box round10)]
    (case p
      :one (sum (for [x (range (inc lx) hx)
                      y (range (inc ly) hy)
                      :when (not (contains? round10 [x y]))]
                  1))
      :two (first (converge-by first round [input dirs])))))
