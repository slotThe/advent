(ns clojure-solutions.day5
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse-crates [crates]
  (->> crates
       str/split-lines
       (drop-last 1)
       (map #(re-seq #"(?:\s{3}|\[\w\])(?:\s?)" %))
       transpose
       (mapv (partial keep #(when-not (str/blank? %) %)))))

(defn- parse-moves [moves]
  (->> moves
       str/split-lines
       (map #(re-find #"move (\d+) from (\d+) to (\d+)" %))
       (map (fn [[_ move from to]]
              {:move (read-string move)
               :from (dec (read-string from))
               :to   (dec (read-string to))}))))

(defn- parse []
  (->> (slurp "./input/day5.txt")
       split-groups
       ((fn [[crates moves]]
          {:crates (parse-crates crates)
           :moves (parse-moves moves)}))))

(defn- do-move [tamper crates {:keys [move from to]}]
  (let [row-from (nth crates from)
        row-to (nth crates to)]
    (assoc crates
           from (drop move row-from)
           to   (concat (tamper (take move row-from))
                        row-to))))

(defn day5 [part]
  (let [{:keys [crates moves]} (parse)]
    (letfn [(solve [tamper]
              (->> (reduce (partial do-move tamper) crates moves)
                   (map first)
                   str/join))]
      (str/replace (case part
                     :one (solve reverse)
                     :two (solve identity))
                   #" |\[|\]"
                   ""))))
