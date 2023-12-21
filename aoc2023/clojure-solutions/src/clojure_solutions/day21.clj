(ns clojure-solutions.day21
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]
            [clojure-aoc-util.coords :as c]))

(defn- parse []
  (str/split-lines (slurp "../inputs/day21.txt")))

(defn- solve [start max-step grid size]
  (letfn [(pull-back [[x y]]
            ;; Treat points as being in a different spot,
            ;; unconstrained by the grid.
            [(mod x size) (mod y size)])]
    (->> (aoc/dijkstra
          start
          (fn [[pt cost] seen]
            (if (> cost (inc max-step))
              []
              (->> (c/neighbours4 pt)
                   (filter (fn [neigh]
                             (and (aoc/not-in? seen neigh)
                                  (contains?
                                   #{\.\S}
                                   (aoc/mat-ix grid (pull-back neigh))))))
                   (map (fn [el] [el 1])))))
          :with-cost)
         ;; Either an exact fit, or close enough that we can reach it with
         ;; some jiggling.
         (filter (fn [[_ n]]
                   (or (= n max-step)
                       (and (< n max-step) ;
                            (= 0 (mod (- max-step n) 2))))))
         count)))

(defn day21 [p]
  (let [grid (parse)
        [[start _]] (filter (comp #(= \S %) second) (c/seq->map grid))
        size (count grid)
        half (quot size 2)]
    (case p
      :one (solve start 64 grid size)
      ;; Oh my god look at the shape of the input. Tacking it together just
      ;; makes it repeat. And we start right in the middle of the diamond.
      ;; Urgh.
      :two (letfn [(go [k] (solve start k grid size))]
             (let [a (go half)
                   t1 (go (+ half size))
                   t2 (go (+ half (* size 2)))
                   b (- t1 a)
                   c (/ (- (- t2 t1) b) 2)
                   x (quot 26501365 size)]
               (+ a (* b x) (* x (- x 1) c)))))))
