(ns clojure-solutions.day05
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :refer [split-groups words]]))

;; Idea:
;;
;;  1. Transform the triples (e.g., `50 98 8') into source
;;     intervals (e.g., [98…105]).
;;
;;  2. Intersect them with a seed interval
;;     (e.g., [100…120] ∩ [98…105] ≡ [100…105])
;;
;;  3. Take care of the resulting transformation according to the
;;     destination (e.g., [100…105] → [52…57]).
;;
;; We have to be a bit careful in that these intersection can be partial: part
;; of the seed interval may intersect with triple n, but another part only
;; does with triple m. This means that we have to find out which bit didn't
;; intersect, and thread it through to the other triples (ones we haven't
;; looked at yet).

(defn- parse []
  (->> (slurp "../inputs/day05.txt")
       split-groups
       (map (comp (partial map words) str/split-lines))
       (map (partial map (partial keep parse-long)))
       (mapv (partial filterv not-empty))
       ((fn [[seeds & rest]]
          [(first seeds), rest]))))

(defn- into-interval
  "Construct an interval starting at s of length l."
  [[s l]]
  [s (+ s (dec l))])

(defn- intersect [[s1 e1] [s2 e2]]
  (when (<= (max s1 s2) (min e1 e2)) ; Is there an intersection?
    [(max s1 s2) (min e1 e2)]))

(defn- subtract
  "Subtract one interval from another. This assumes that the intervals
  intersect in some way."
  [[s1 e1] [s2 e2]]
  (when-not (and (<= s2 s1) (<= e1 e2)) ; completely contained
    (let [[s e] (if (< s1 s2)
                  [s1                (min (dec s2) e1)]
                  [(max s1 (inc e2)) e1               ])]
      (when-not (< e s)                 ; not a real interval
        [s e]))))

(defn- calc-intersections
  [source-ival almanac-maps]
  (loop [res [], ival source-ival, maps almanac-maps]
    (cond
      (nil? ival)    res                ; ran out of source interval
      (empty? maps) (conj res ival)     ; ran out of dest intervals
      :else
      (let [[[dest source len] & ts] maps
            ival2 (into-interval [source len])]
        (if-let [[s e] (intersect ival ival2)]
          ;; Some intersection was found, so add the translated interval to
          ;; the result, and try to find more intersections on any bit that
          ;; wasn't in an intersection yet.
          (recur (conj res (into-interval [(+ s (- dest source))
                                           (inc (- e s))]))
                 (subtract ival ival2)
                 ts)
          ;; No intersection found, so ignore this map.
          (recur res ival ts))))))

(defn day05 [p]
  (let [[seeds almanac-maps] (parse)
        seed-vals (case p
                    :one (map #(into-interval [% 1]) seeds)
                    :two (map into-interval (partition 2 seeds)))]
    (->> (reduce (fn [seeds trans]
                   (mapcat #(calc-intersections % trans) seeds))
                 seed-vals
                 almanac-maps)
         (map first)
         (apply min))))
