(ns clojure-solutions.day9
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day9.txt")
       str/split-lines
       (mapcat (fn [move]
                 (let [[mv amount] (words move)]
                   ;; Expand moves to individual instructions.
                   (repeat (read-string amount) (keyword mv)))))))

(defn- move [[z w] dir]
  (case dir
    :L [(- z 1) w]
    :R [(+ z 1) w]
    :U [z       (+ w 1)]
    :D [z       (- w 1)]))

(defn- follow [[tx ty] [hx hy]]
  (let [dx (- hx tx), dy (- hy ty)
        ax (abs dx), ay (abs dy)]
    (cond
      (and (<= ax 1) (<= ay 1)) [tx ty]                     ; directly adjacent
      (> ax ay)                 [(- hx (signum dx)), hy]    ; horizontally estranged
      (> ay ax)                 [hx, (- hy (signum dy))]    ; vertically estranged
      true                      [(- hx (signum dx)),        ; diagonally estranged
                                 (- hy (signum dy))])))

(defn- simulate [ms]
  (iterate #(reductions follow [0 0] %) (reductions move [0 0] ms)))

(defn day9 [p]
  (count
   (into #{}
         (nth (simulate (parse))
              (case p
                :one 1
                :two 9)))))
