(ns clojure-solutions.day13
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]))

(defn- parse []
  (->> (slurp "../inputs/day13.txt")
       aoc/split-groups
       (map (comp aoc/transpose       ; Flip so `find-split' checks horizontal splits first.
                  (partial map seq)
                  str/split-lines))))

(defn- find-reflection
  "Find a reflection. Eq is the equality comparison to be used. Ground is
  a list of ground to cover, which are in ordinary matrix form (i.e.,
  going down the columns increments y)."
  [eq ground]
  (loop [n 1]
    (if (>= n (count ground))
      ;; We assume that a reflection exists, so this will be at most one
      ;; recursive call.
      (* 100 (find-reflection eq (aoc/transpose ground))) ; Multiply rows by 100
      (let [[pre suf] (split-at n ground)
            pre (reverse pre)
            [min max] (sort-by count [pre suf])]
        (if (eq min (drop-last (- (count max) (count min))
                               max))
          n
          (recur (+ n 1)))))))

;; Since every mirror is assumed to have *exactly one* smudge, no current
;; reflection line can still be one, so we don't have to do anything fancy.
(defn- =-up-to-smudge [coll1 coll2]
  (= 1 (aoc/sum
        (map (fn [a b] (if (= a b) 0 1))
             (flatten coll1) (flatten coll2)))))

(defn day13 [p]
  (letfn [(solve [eq inp]
            (aoc/sum (map (partial find-reflection eq) inp)))]
    (case p
      :one (solve =              (parse))
      :two (solve =-up-to-smudge (parse)))))
