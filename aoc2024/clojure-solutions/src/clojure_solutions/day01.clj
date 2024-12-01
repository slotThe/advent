(ns clojure-solutions.day01
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util])
  (:gen-class))

(defn- parse []
  (->> (slurp "../inputs/day01.txt")
       str/split-lines
       (map (comp (partial map read-string)
                  #(str/split % #" +")))
       util/transpose
       (map sort)))

(defn -main [& _args]
  (let [[a b] (parse)
        freqs (frequencies b)]
    (util/print-day
     1
     (reduce + (map (comp abs -) a b))
     (reduce + (for [x a] (* x (get freqs x 0)))))))
