(ns clojure-solutions.day10
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]
            [clojure-aoc-util.coords :as c]))

(defn- find-loop [grid]
  (letfn [(get-connection [[[x y] pipe]]
            (case pipe
              \| [[(- x 1) y] [(+ x 1) y]] ; | n s
              \- [[x (+ y 1)] [x (- y 1)]] ; - e w
              \L [[(- x 1) y] [x (+ y 1)]] ; └ n e
              \J [[(- x 1) y] [x (- y 1)]] ; ┘ n w
              \7 [[(+ x 1) y] [x (- y 1)]] ; ┐ s w
              \F [[(+ x 1) y] [x (+ y 1)]] ; ┌ s e
              \S [[(+ x 1) y] [x (+ y 1)]] ; ┌ s e
              \. []))]
    (let [start (some (fn [[xy s]] (when (= s \S) xy))
                      (aoc/map-matrix (fn [i j el] [[i j] el]) grid))]
      ;; Simple BFS to find path lengths.
      (loop [res [], seen #{}, work [[start \S 0]]]
        (if (empty? work)
          res
          (let [[pos pipe len] (first work)]
            (recur (conj res [pos len])
                   (conj seen pos)
                   (concat (rest work)
                           (keep (fn [pos]
                                   (when (aoc/not-in? seen pos)
                                     [pos (aoc/mat-ix grid pos) (inc len)]))
                                 (get-connection [pos pipe]))))))))))

(defn- widen
  "Widen the grid. Use a unique symbol (#) to signify ground that is
  enough to squeeze through in the original grid, but not actual
  ground."
  [grid]
  (letfn [(do-widen [f xs]
            (map (partial mapcat f) xs))]
    (->> grid
         ;; Widen horizontally.
         (do-widen (fn [x]
                     ;; We can't squeeze through walls that lock into each
                     ;; other. In this case, we don't even need to know what
                     ;; symbol comes next, because we only care about the main
                     ;; loop. A symbol like L can't be followed horizontally
                     ;; by |, as they don't fit together. In other words,
                     ;; right-extending symbols can't be passed through, but
                     ;; all others (walls *and* ground) can.
                     (if (contains? #{\- \F \L} x) [x \-] [x \#])))
         ;; Widen vertically.
         aoc/transpose
         (do-widen (fn [x]
                     ;; Squeezing vertically works in the same way.
                     (if (contains? #{\| \F \7} x) [x \|] [x \#])))
         aoc/transpose)))

(defn- clean
  "Clean the grid. That is, transform all rubbish outside of the main loop
  into ground, and the start symbol into a ┌."
  [grid]
  (let [loop (into #{} (map first) (find-loop grid))]
    (aoc/map-over-matrix
     (fn [i j el]
       (cond
         (= el \S)                \F
         (aoc/not-in? loop [i j]) \.
         :else                    el))
     grid)))

(defn- pad-grid [grid padder]
  (let [grid (widen grid)
        len (+ 2 (count (first grid)))
        grid (concat [(repeat len padder)]
                     (map (fn [row] (concat [padder] row [padder])) grid)
                     [(repeat len padder)])]
    (concat [(repeat len padder)]
            (map (fn [row] (concat [padder] row [padder])) grid)
            [(repeat len padder)])))

(defn- part2 [grid]
  (let [grid (pad-grid (clean grid) \#)
        all-floor (count (filter #(= \. %) (apply concat grid)))]
    (->> (aoc/flood-fill                ; DFS to find all ground spots
          [[0 0] \#]
          (fn [[pos _] seen]
            (keep (fn [neigh]
                    (when-let [pipe (aoc/mat-ix grid neigh)]
                      (when (and (aoc/not-in? seen [neigh pipe])
                                 (contains? #{\. \#} pipe))
                        [neigh pipe])))
                  (c/neighbours4 pos))))
         (filter (comp (partial = \.) second)) ; Get outsides floor
         count                                 ; Count :)
         (- all-floor)                         ; Contrast with all floor
         )))

(defn day10 [p]
  (let [grid (mapv vec (str/split-lines (slurp "../inputs/day10.txt")))]
    (case p
      :one (apply max (map second (find-loop grid)))
      :two (part2 grid))))
