(ns clojure-solutions.day6
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (map read-string (str/split (slurp "./input/day6.txt") #",")))

(defn- solve [coll]
  (let [template-map {0 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0, 8 0}]
    (iterate
     (fn [xs]
       (reduce (fn [m [k v]]
                 (if (= k 0)
                   ;; fish giving birth -> spawns `v' new fish, needs a
                   ;; break of 6 days
                   (update (update m 6 #(+ v %)) 8 #(+ v %))
                   ;; normal fish, not giving birth -> just decrement
                   (update m (dec k) #(+ v %))))
               template-map
               xs))
     (frequencies coll))))

(defn day6 []
  (let [input (parse)
        day (fn [n]
              (reduce (fn [acc [_ v]] (+ acc v))
                      0
                      (nth (solve input) n)))]
    ;; part 1 => 345793
    (println (day 80))
    ;; part 2 => 1572643095893
    (println (day 256))))
