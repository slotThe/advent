(ns clojure-solutions.day12
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure-aoc-util.util :as aoc]))

(defn- parse []
  (->> (slurp "../inputs/day12.txt")
       str/split-lines
       (map (comp (fn [[springs damaged]]
                    [springs, (read-string (str "[" damaged "]"))])
                  aoc/words))))

;; As always, memoization saves the day.
(def check
  (letfn [(non#?    [sym] (contains? #{\?\.} sym))
          (working? [sym] (contains? #{\#\?} sym))
          (check-memo
            ([[springs damaged]] (check-memo (seq springs) damaged))
            ([springs damaged]
             (cond
               (empty? damaged) (if (every? non#? springs) 1 0)
               (empty? springs ) 0
               :else
               (let [dh (first damaged), dr (rest damaged)]
                 (match springs
                   ([\. & sr] :seq) (check sr damaged)
                   ([\# & _ ] :seq) (let [[pre suf] (split-at dh springs)]
                                      (if (and (= dh (count pre))    ; As long as required
                                               (every? working? pre) ; Valid consecutive sequence
                                               (or (empty? suf)      ; Empty or followed by a non-# char
                                                   (non#? (first suf))))
                                        (check (rest suf) dr)
                                        0))
                   ([\? & sr] :seq) (+ (check sr damaged) ; Omit . as it will be killed in the next step
                                       (check (cons \# sr) damaged)))))))]
    (memoize check-memo)))

(defn- unfold [[springs damaged]]
  [(str/join "?" (repeat 5 springs))
   (apply concat (repeat 5 damaged))])

(defn day12 [p]
  (case p
    :one (aoc/sum (map       check         (parse)))
    :two (aoc/sum (map (comp check unfold) (parse)))))
