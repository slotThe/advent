(ns clojure-solutions.day4
  (:require [clojure.string :as str]))

(defn- completely-overlap [{[e11 e12] :elf1, [e21 e22] :elf2}]
  (or (and (<= e21 e11) (<= e12 e22))
      (and (<= e11 e21) (<= e22 e12))))

(defn- overlap [{[e11 e12] :elf1, [e21 e22] :elf2}]
  (not (or (< e12 e21) (< e22 e11))))

(defn- parse []
  (->> (slurp "./input/day4.txt")
       str/split-lines
       (map #(re-find #"(\d+)-(\d+),(\d+)-(\d+)" %))
       (map #(drop 1 %))                ; Drop whole match
       (map #(map read-string %))
       (map (fn [[e11 e12 e21 e22]]
              {:elf1 [e11 e12]
               :elf2 [e21 e22]}))))

(defn- solve [filter-by]
  (count (filter filter-by (parse))))

(defn day4 [part]
  (case part
    :one (solve completely-overlap)
    :two (solve overlap)))
