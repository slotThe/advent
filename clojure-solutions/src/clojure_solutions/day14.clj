(ns clojure-solutions.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse []
  (letfn [(line [[[a b] [c d]]]
            (for [x (range (min a c) (+ 1 (max a c)))
                  y (range (min b d) (+ 1 (max b d)))]
              [x y]))]
    (->> (slurp "../inputs/day14.txt")
         str/split-lines
         (map #(re-seq #"(\d+),(\d+)" %))
         (map #(map (fn [[_ n m]]
                      [(read-string n) (read-string m)])
                    %))
         (mapcat #(partition 2 1 %))
         (into #{} (mapcat line)))))

(defn- fall [cave max-depth bail]
  (letfn [(step [[x y]]
            (first
             (for [p [[x (inc y)]
                      [(dec x) (inc y)]
                      [(inc x) (inc y)]]
                   :when (not (contains? cave p))]
               p)))]
    (loop [pos [500 0]]
      (let [p (step pos)]
        (cond (nil? p) pos
              (> (second p) max-depth) (bail p)
              :else (recur p))))))

(defn- solve [inp def fail]
  (let [max-depth (second (first (sort-by second > inp)))]
    (loop [cave inp]
      (let [new-thing (fall cave max-depth def)]
        (if (fail cave new-thing)
          cave
          (recur (conj cave new-thing)))))))

(defn day14 [p]
  (let [inp (parse)]
    (count (set/difference
            (case p
              :one (solve inp (constantly nil) (fn [_ new] (nil? new)))
              :two (solve inp identity (fn [cave new] (contains? cave new))))
            inp))))
