(ns clojure-solutions.day22
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:use [clojure-solutions.util] :reload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defn- parse []
  (->> (slurp "../inputs/day22.txt")
       split-groups
       ((fn [[board moves]]
          {:board (into {}
                        (filter (fn [[_ el]] (not= \space el)))
                        (map-matrix (fn [i j el] [[j i] el])
                                    (str/split-lines board)))
           :moves (map #(match %
                          "L" :L
                          "R" :R
                          n   (read-string n))
                       (re-seq #"\d+|L|R" moves))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D coordinates (for next year I'll write a library!)

(defn- add-coords ^longs [^longs [x1 y1] ^longs [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- sub-coords ^longs [^longs [x1 y1] ^longs [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn- dir->kw [dir]
  (case dir
    [1  0] :east
    [-1 0] :west
    [0  1] :south
    [0 -1] :north))

(defn- kw->dir [kw]
  (case kw
    :east  [1  0]
    :west  [-1 0]
    :south [0  1]
    :north [0 -1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving about

(defn- get-start [board]
  (->> board
       (filter (fn [[[_ j] _]] (= 0 j)))
       (apply min-key ffirst)
       first))

(defn- change-direction [dir looking]
  (let [dirs [[0 -1] [-1 0] [0 1] [1 0]]
        pos  (.indexOf dirs looking)]
    (case dir
      :L (nth dirs (mod (inc pos) 4))
      :R (nth dirs (mod (dec pos) 4)))))

(defn- move [transition-with {:keys [board moves]}]
  (reduce (fn [[pos dir] m]
            (match m
              (kw :guard keyword?) [pos (change-direction kw dir)]
              n                    (transition-with board n pos dir)))
          [(get-start board) [1 0]]
          moves))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1

(defn- calc-pos [board rounds pos dir]
  (loop [n rounds, p pos]
    (let [next (add-coords p dir)
          new-p (if (get board next)
                  next
                  (last (take-while
                         (partial contains? board)
                         (iterate #(sub-coords % dir) p))))]
      (if (or (= 0 n) (= \# (get board new-p)))
        p
        (recur (dec n) new-p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cube shit (part 2)

(defn- get-face [[x y]]
  (case [(quot x 50) (quot y 50)]
    [1 0] 1
    [2 0] 2
    [1 1] 3
    [0 2] 5
    [1 2] 4
    [0 3] 6
    :otherwise :not-found))

(defn- calc-pos-dir
  "The layout of the cube looks like this:

               12
               3
              54
              6

  Hence, we just hard code the relevant transitions."
  [board n p d]
  (loop [rounds n, [x y :as pos] p, dir d]
    (if (= 0 rounds)
      [pos dir]
      (let [next (add-coords pos dir)
            [new-pos new-dir]
            (if (contains? board next)  ; Do we have to make a transition?
              [next (dir->kw dir)]
              (let [x-ov   (mod x 50)   ; x pos (overflow) on current face
                    y-ov   (mod y 50)   ; y pos (overflow) on current face
                    flip-y (- 49 y-ov)] ; Sometimes edges get flipped when folding
                (case [(get-face pos) (dir->kw dir)]
                  [1 :north] [[0            (+ 150 x-ov)  ] :east ]
                  [1 :west ] [[0            (+ 100 flip-y)] :east ]
                  [2 :north] [[x-ov         199           ] :north]
                  [2 :east ] [[99           (+ 100 flip-y)] :west ]
                  [2 :south] [[99           (+ 50 x-ov)   ] :west ]
                  [3 :east ] [[(+ 100 y-ov) 49            ] :north]
                  [3 :west ] [[y-ov         100           ] :south]
                  [4 :east ] [[149          flip-y        ] :west ]
                  [4 :south] [[49           (+ 150 x-ov)  ] :west ]
                  [5 :north] [[50           (+ 50 x-ov)   ] :east ]
                  [5 :west ] [[50           flip-y        ] :east ]
                  [6 :east ] [[(+ 50 y-ov)  149           ] :north]
                  [6 :south] [[(+ 100 x-ov) 0             ] :south]
                  [6 :west ] [[(+ 50 y-ov)  0             ] :south])))]
        (if (= \# (get board new-pos))
          [pos dir]
          (recur (dec rounds) new-pos (kw->dir new-dir)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve

(defn- score [[[c r] d]]
  (+ (* 1000 (inc r))
     (* 4    (inc c))
     (match d
       [1  0] 0
       [0 -1] 1
       [-1 0] 2
       [0  1] 3)))

(defn day22 [p]
  (let [transition-with
        (case p
          :one (fn [board n pos dir]
                 [(calc-pos board n pos dir) dir])
          :two calc-pos-dir)]
    (score (move transition-with (parse)))))
