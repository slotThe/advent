(ns clojure-solutions.day03
  (:require [clojure.string :as str]
            [clojure-aoc-util.coords :as coord]
            [clojure-aoc-util.util :refer [coll-to-base sum]]))

(defn- mk-adjacency-map [inp]
  (letfn [(is-num [n] (contains? #{\0\1\2\3\4\5\6\7\8\9} n))
          (is-sym [n] (and n (not (or (= n \.) (is-num n)))))
          (val    [n] (get inp n))]
    (apply
     merge-with concat
     (for [[ix el] inp
           :when (and (is-num el)                           ; a number
                      (not (is-num (val (coord/left ix))))) ; leftmost digit
           :let [all-digits (take-while (comp is-num val) (iterate coord/right ix))
                 chosen-one (some #(when (is-sym (val %)) %)
                                  (mapcat coord/neighbours8 all-digits))]
           :when chosen-one]
       {chosen-one
        [(coll-to-base 10 (map val all-digits))]}))))

(defn day03 [p]
  (let [inp (coord/seq->map
             (mapv vec (str/split-lines (slurp "../inputs/day03.txt"))))]
    (sum (case p
           :one (apply concat (vals (mk-adjacency-map inp)))
           :two (map (fn [[_ [a b]]] (* a b))
                     (filter (fn [[ix nums]]
                               (and (= (get inp ix) \*)
                                    (= 2 (count nums))))
                             (mk-adjacency-map inp)))))))
