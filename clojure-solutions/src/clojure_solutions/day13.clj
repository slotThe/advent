(ns clojure-solutions.day13
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "../inputs/day13.txt")
       str/split-lines
       (filter (partial not= ""))
       (map read-string)))   ; THANK YOU CLOJURE

(defn- cmp [a b]
  (match [a b]
         [[a*] [b*]] (cmp a* b*)
         [a* b*]
         (case [(number? a*) (number? b*)]
           [true  true ] (compare a* b*)
           [true  false] (cmp [a*] b*)
           [false true ] (cmp a* [b*])
           [false false] (reduce (fn [a b] (if (= a 0) b a))
                                 (conj (mapv cmp a* b*)
                                       (compare (count a*) (count b*)))))))

(defn day13 [p]
  (let [inp (parse)]
    (case p
      :one (->> inp                                        ; ( [3] [[4] 5] … )
                (partition 2)                              ; ( ( [3] [[4] 5] ) … )
                (map (fn [[a b]] (cmp a b)))               ; ( -1 1 -1 … )
                (map (fn [n o] (if (= -1 o) n 0))          ; (  1 0  3 … )
                     (iterate inc 1))
                sum)
      :two (->> inp                                        ; ( [3] [[4] 5] … )
                (concat [[[2]]] [[[6]]])                   ; ( [[2]] [[6]] [3] [[4] 5] … )
                (sort cmp)                                 ; ( [] [[]] … )
                (map vector (iterate inc 1))               ; ( [1 []] [2 [[]]] … )
                (filter (fn [[_ b]] (or (= b [[2]])        ; ( [n [[2]]] [m [[6]]]
                                        (= b [[6]]))))
                (reduce (fn [acc [a _]] (* acc a)) 1)))))
