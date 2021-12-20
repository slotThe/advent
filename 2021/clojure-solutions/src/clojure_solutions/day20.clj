(ns clojure-solutions.day20
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (letfn [(char->num [c]
            (case c
              \. 0
              \# 1))]
    (let [[enhancement input] (split-groups (slurp "./input/day20.txt"))]
      {:enhancement (mapv char->num enhancement)
       :grid (->> (str/split-lines input)
                  (mapv (partial mapv char->num))
                  (map-matrix (fn [i j el] {[j i] el}))
                  (apply merge))})))

(defn- around [i j]
  (for [x [-1 0 1], y [-1 0 1]]
    [(+ i y) (+ j x)]))

(defn- grow [grid]
  (let [[mn mx] ((fn [[x y]] [(dec x) (inc y)])
                 (reduce (fn [[xmin xmax] [x y]]
                           [(min xmin x) (max xmax x)])
                         grid))]
    (mapcat (fn [x] [[x mn] [mx x] [x mx] [mn x]])
            (range mn (inc mx)))))

(defn- step [{:keys [enhancement grid]}]
  (map first
       (iterate
        (fn [[g n]]
          (let [keys-g (keys g)
                ;; If the first bit is one, the entire infinite grid
                ;; will be "lit up" on every even iteration.
                default (if (and (= 1 (enhancement 0)) (even? n))
                          1
                          0)]
            (letfn [(lookup [x y]
                      (->> (around x y)
                           (map #(get g % default)) ; look up state of neighbours
                           (coll-to-base 2) ; into decimal number for lookup
                           enhancement))]   ; look up up new state
              [(persistent!
                (reduce (fn [acc [x y]] (assoc! acc [x y] (lookup x y)))
                        (transient {})
                        (concat keys-g (grow keys-g))))
               (inc n)])))
        [grid 1])))

(defn day20 []
  (let [steps (step (parse))
        score (fn [n] (->> (nth steps n)
                           (filter-val (partial = 1))
                           count))]
    (println (score 2))
    (println (score 50))))
