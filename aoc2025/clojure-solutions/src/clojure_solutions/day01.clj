(ns clojure-solutions.day01
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util]
            [clojure.math :as m])
  (:gen-class))

(defn- parse []
  (->> (slurp "../inputs/day01.txt")
       str/split-lines
       (map (fn [l]
              (read-string
               (str/replace l
                            #"(L)|(R)"
                            (fn [m] (if (= (first m) "L") "-" ""))))))))

(defn- one [xs]
  (->> (reductions (fn [d n] (mod (+ d n) 100)) 50 xs)
       (filter #(= 0 %))
       count))

(defn- two [xs]
  (->> xs
       (reduce
        (fn [[z d] n]
          [(+ z (quot (+ (abs n) (mod (* (m/signum n) d) 100)) 100))
           (mod (+ d n) 100)])
        [0 50])
       first
       int))

(defn -main [& _args]
  (let [inp (parse), f (one inp), s (two inp)]
    (assert (and (= 1011 f) (= 5937 s)))
    (util/print-day 1 f s)))
