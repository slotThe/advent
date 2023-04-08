(ns clojure-solutions.day3
  (:require [clojure.string :as str]
            [clojure.set    :as set])
  (:use [clojure-aoc-util.util] :reload))

(defn- parse []
  (str/split-lines (slurp "../inputs/day3.txt")))

(defn- value [c]
  (let [ascii (int c)]
    (cond
      (<= 97 ascii 122) (- ascii 96)
      (<= 65 ascii 90 ) (- ascii 38))))

(defn- solve [fun]
  (->> (parse)                           ; ["AA" "acD" …]
       fun                               ; [[[\A \A] …] [[\c \Z \a] …] …]
       (map #(map (partial into #{}) %)) ; [[#{\A} …] [#{\c \Z \a} …] …]
       (map #(apply set/intersection %)) ; [#{\A} #{\Z} …]
       (map (comp value first))          ; (+ 27 52 …)
       sum))

(defn day3 [part]
  (case part
    :one (solve #(map (fn [s] (split-at (/ (count s) 2) s)) %))
    :two (solve #(partition 3 %))))
