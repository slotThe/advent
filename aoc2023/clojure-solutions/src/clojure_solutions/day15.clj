(ns clojure-solutions.day15
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]))

(defn- parse []
  (str/split (first (str/split-lines (slurp "../inputs/day15.txt"))) #","))

(defn- HASH [coll]
  (reduce (fn [acc el] (rem (* 17 (+ acc (int el))) 256)) 0 coll))

(defn- part2 [hm ins]
  (let [[label assign] (str/split ins #"=|-")
        box (HASH label)]
    (if assign
      (let [focal-length (read-string assign)]
        ;; Reassign
        (update hm box (fn [labels]
                         (if (aoc/elem label (map first labels))
                           (mapv (fn [[lbl fl]]
                                   (if (= lbl label) [lbl focal-length] [lbl fl]))
                                 labels)
                           (conj (or labels []) [label focal-length])))))
      ;; Delete
      (update hm box (partial filterv #(not= label (first %)))))))

(defn day15 [p]
  (aoc/sum
   (case p
     :one (map HASH (parse))
     :two (mapcat (fn [[box lenses]]
                    (map (fn [i [_ fl]]
                           (* (inc box) (inc i) fl))
                         (range)
                         lenses))
                  (reduce part2 {} (parse))))))
