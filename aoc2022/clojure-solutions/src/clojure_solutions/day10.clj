(ns clojure-solutions.day10
  (:require [clojure.string :as str])
  (:use [clojure-aoc-util.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day10.txt")
       str/split-lines
       (map (fn [ins]
              (let [[_ a] (words ins)]
                (if a [:add (read-string a)] :noop))))))

(defn- simulate [inp]
  (letfn [(step [x i]
            (if (= :noop i) [x] [x (+ x (second i))]))]
    (apply concat
           (reductions (fn [acc ins] (step (peek acc) ins))
                       [1]
                       inp))))

(defn- part-two [inp]
  (letfn [(draw-pixel? [pos sprite]
            (<= (dec sprite) (mod pos 40) (inc sprite)))]
    (let [pixels (map (fn [row pos]
                        (if (draw-pixel? row pos) "█" " "))
                      (range 0 (count inp))
                      inp)]
      (->> pixels
           (partition 40)
           (map str/join)
           (cons "\n")
           (str/join "\n")))))

(defn- part-one [inp]
  (letfn [(calc [n] (* n (nth inp (dec n))))]
    (reduce (fn [acc e] (+ acc (calc e)))
            0
            [20 60 100 140 180 220])))

(defn day10 [p]
  ((case p
     :one part-one
     :two part-two)
   (simulate (parse))))
