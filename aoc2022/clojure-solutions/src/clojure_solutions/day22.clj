(ns clojure-solutions.day22
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure-aoc-util.coords :as coords])
  (:use [clojure-aoc-util.util] :reload))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving about

(defn- get-start [board]
  (->> board
       (filter (fn [[[_ j] _]] (= 0 j)))
       (apply min-key ffirst)
       first))

(defn- move [transition-with {:keys [board moves]}]
  (reduce (fn [[pos dir] m]
            (match m
              (turn :guard keyword?) [pos (coords/turn turn dir)]
              n                      (transition-with board n pos dir)))
          [(get-start board) coords/east]
          moves))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1

(defn- calc-pos [board rounds pos dir]
  (loop [n rounds, p pos]
    (let [next (coords/add p dir)
          new-p (if (get board next)
                  next
                  (last (take-while
                         (partial contains? board)
                         (iterate #(coords/sub % dir) p))))]
      (if (or (= 0 n) (= \# (get board new-p)))
        p
        (recur (dec n) new-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cube shit (part 2)

(defn- get-face
  "The layout of the cube looks like this:

               12
               3
              54
              6"
  [[x y]]
  (case [(quot x 50) (quot y 50)]
    [1 0] 1
    [2 0] 2
    [1 1] 3
    [0 2] 5
    [1 2] 4
    [0 3] 6
    :otherwise :not-found))

(defn- calc-pos-dir
  "Just hard code the relevant transitions."
  [board n p d]
  (loop [rounds n, [x y :as pos] p, dir d]
    (if (= 0 rounds)
      [pos dir]
      (let [next (coords/add pos dir)
            [new-pos new-dir]
            (if (contains? board next)  ; Do we have to make a transition?
              [next (coords/dir->kw dir)]
              (let [x-ov   (mod x 50)   ; x pos (overflow) on current face
                    y-ov   (mod y 50)   ; y pos (overflow) on current face
                    flip-y (- 49 y-ov)] ; Sometimes edges get flipped when folding
                (case [(get-face pos) (coords/dir->kw dir)]
                  [1 :N] [[0            (+ 150 x-ov)  ] :E]
                  [1 :W] [[0            (+ 100 flip-y)] :E]
                  [2 :N] [[x-ov         199           ] :N]
                  [2 :E] [[99           (+ 100 flip-y)] :W]
                  [2 :S] [[99           (+ 50 x-ov)   ] :W]
                  [3 :E] [[(+ 100 y-ov) 49            ] :N]
                  [3 :W] [[y-ov         100           ] :S]
                  [4 :E] [[149          flip-y        ] :W]
                  [4 :S] [[49           (+ 150 x-ov)  ] :W]
                  [5 :N] [[50           (+ 50 x-ov)   ] :E]
                  [5 :W] [[50           flip-y        ] :E]
                  [6 :E] [[(+ 50 y-ov)  149           ] :N]
                  [6 :S] [[(+ 100 x-ov) 0             ] :S]
                  [6 :W] [[(+ 50 y-ov)  0             ] :S])))]
        (if (= \# (get board new-pos))
          [pos dir]
          (recur (dec rounds) new-pos (coords/kw->dir new-dir)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve

(defn- score [[[c r] d]]
  (+ (* 1000 (inc r))
     (* 4    (inc c))
     (case (coords/dir->kw d)
       :E 0
       :S 1
       :W 2
       :N 3)))

(defn day22 [p]
  (let [transition-with
        (case p
          :one (fn [board n pos dir]
                 [(calc-pos board n pos dir) dir])
          :two calc-pos-dir)]
    (score (move transition-with (parse)))))
