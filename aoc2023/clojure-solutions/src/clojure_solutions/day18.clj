(ns clojure-solutions.day18
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]))

(defn- parse1 []
  (->> (slurp "../inputs/day18.txt")
       str/split-lines
       (map aoc/words)
       (mapv (fn [[dir amnt _]]
               [(keyword dir), (read-string amnt)]))))

(defn- parse2 []
  (->> (slurp "../inputs/day18.txt")
       str/split-lines
       (map (partial re-find #"#((\d|\w)+)"))
       (map (fn [[_ s _]]
              (let [dir (case (last s)
                          \0 :R, \1 :D, \2 :L, \3 :U)
                    amnt (read-string (apply str (concat "0x" (drop-last s))))]
                [dir amnt])))))

(defn- solve
  "The idea is to eagerly fill out every square while moving either left
  or right. Giving them different signs means that they can cancel each
  other out when \"ditches\" are being created. After that, we just have
  to make sure to remember that these trenches are thickened (have a
  width) and correct for that."
  [moves]
  (let [[_ a l] (reduce
                 (fn [[[x y] acc len] [dir n]]
                   (case dir
                     :R [[(+ x n) y] (+ acc (* y n)) (+ len n)] ; Add    square
                     :L [[(- x n) y] (- acc (* y n)) (+ len n)] ; Remove square
                     :U [[x (- y n)] acc             (+ len n)]
                     :D [[x (+ y n)] acc             (+ len n)]))
                 [[0 0] 0 0]
                 moves)]
    (+ (abs a) (inc (/ l 2)))))

(defn day18 [p]
  (case p
    :one (solve (parse1))
    :two (solve (parse2))))
