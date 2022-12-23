(ns clojure-solutions.day23
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day23.txt")
       str/split-lines
       (map-matrix (fn [i j el]
                     (when (= el \#) [j i])))
       (into #{})))

(defn- neighbours [f [x y]]
  (filter f
          {:NW [(dec x) (dec y)] :N [x (dec y)] :NE [(inc x) (dec y)]
           :W  [(dec x)      y ]                :E  [(inc x)      y ]
           :SW [(dec x) (inc y)] :S [x (inc y)] :SE [(inc x) (inc y)]}))

(defn- propose-spot [neighs [x y] dir]
  (let [s (case dir
            :N #{:N :NE :NW}
            :S #{:S :SE :SW}
            :E #{:W :NW :SW}
            :W #{:E :NE :SE})]
    (when (not-any? s neighs)
      (case dir
        :N [x (dec y)]
        :S [x (inc y)]
        :E [(dec x) y]
        :W [(inc x) y]))))

(defn- propose-move [scan dirs [x y]]
  (let [neighs (keys (neighbours (fn [[k v]] (contains? scan v))
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

(defn- bounding-box [hs]
  (letfn [(go [op]
              (reduce (fn [[lx ly] [x y]]
                        [(op lx x) (op ly y)])
                      hs))]
    [(map dec (go min)) (map inc (go max))]))

(defn day23 [p]
  (let [input (parse)
        dirs [:N :S :E :W]
        round10 (first (nth (iterate round [input dirs]) 10))
        [[lx ly] [hx hy]] (bounding-box round10)]
    (case p
      :one (sum (for [x (range (inc lx) hx)
                      y (range (inc ly) hy)
                      :when (not (contains? round10 [x y]))]
                  1))
      :two (first
            (converge-by first round [input dirs])))))
