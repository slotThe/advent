(ns clojure-solutions.day08
  (:require [clojure.string :as str]
            [clojure-aoc-util.coords :as coord]
            [clojure.set :as set])
  (:use [clojure-aoc-util.util] :reload))

(defn- parse []
  (let [[lr bin] (split-groups (slurp "../inputs/day08.txt"))]
    [(map #(case %, \L first, \R second) lr)
     (into {}
           (comp (map #(re-seq #"[A-Z]{3}" %))
                 (map (fn [[k l r]] {k [l r]})))
           (str/split-lines bin))]))

(defn- go-down [start ends lr tree]
  (loop [n 0, [choose & is] (cycle lr), node start]
    (if (contains? ends node)
      n
      (recur (inc n) is (choose (get tree node))))))

(defn day08 "Yay, maths!" [p]
  (let [[lr tree] (parse)
        nodes (keys tree)
        start (filter #(str/ends-with? % "A") nodes)
        end   (into #{} (filter #(str/ends-with? % "Z") nodes))]
    (case p
      :one (go-down "AAA" #{"ZZZ"} lr tree)
      :two (reduce lcm (map #(go-down % end lr tree) start)))))
