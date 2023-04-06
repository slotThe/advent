(ns clojure-solutions.day4
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- read-board [xss]
  (->> xss
       (map #(filter not-empty %))      ; numbers are aligned; kill that
       (map #(map read-string %))))

(defn- parse []
  (let [[nums & bs] (split-groups (slurp "./input/day4.txt"))
        draws (map read-string (str/split nums #","))
        boards (map (comp read-board , #(map words %) , str/split-lines)
                    bs)]
    [draws boards]))

(defn- strike-through
  "Mark a number as `drawn' by (sigh) replacing it with -1."
  [n xss]
  (map #(replace {n -1} %) xss))

(defn- score
  "The ending score."
  [n b]
  (* n (sum (filter #(not= -1 %) (apply concat b)))))

(defn- winning-board? [xss]
  (letfn ((winning-col? [board]
            (some #{true}
                  (map #(every? (partial = -1) %) ; -1 because no types :(
                       board))))
    (or (winning-col? xss) (winning-col? (transpose xss)))))

(defn- solve
  "Return the score of all winning boards in order."
  [[numbers all-boards]]
  (loop [[n & ns] numbers
         boards   all-boards
         winners  []]
    (let [boards*                 (map #(strike-through n %) boards)
          {wins true, losers nil} (group-by winning-board? boards*)]
      (if (empty? boards*)
        winners
        ;; Remove winners from active boards if necessary.
        (recur ns losers (concat winners (map #(score n %) wins)))))))

(defn day4 []
  (let [boards (solve (parse))]
    (println (first boards))            ; => 89001
    (println (last boards))))           ; => 7296
