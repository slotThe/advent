(ns clojure-solutions.util
  (:require [clojure.string :as str]))

;;; Parsing

(defn str-to-coll-base
  "Convert a string into a collection, reading every character in the
  given base; e.g, \"12312094\" -> '(1 2 3 1 2 0 9 4) in base 10."
  [b s]
  (map #(Character/digit ^char % ^int b) s))

(defn coll-to-base
  "Convert a collection a number in the given base; e.g.,
  '(1 2 3 1 2 0 9 4) -> `12312094' in base 10."
  [b xs]
  (BigInteger. ^String (apply str xs) ^int b))

(defn words [s]
  (str/split s #" "))

(defn split-groups [s]
  (str/split s #"\n\n"))

;;; Pretty printing

(defn plot-set
  "Plot a set of points of the form [x y]; ASCII style."
  [s]
  (let [[xmax ymax] (reduce (fn [[x-max y-max] [x y]]
                              [(max x-max x) (max y-max y)])
                            s)
        [xmin ymin] (reduce (fn [[x-min y-min] [x y]]
                              [(min x-min x) (min y-min y)])
                            s)]
    (map (fn [line] (str/join
                     (map (fn [[a b]] (if (s [a b]) "█" " "))
                          line)))
         (map (fn [y] (map #(vector % y)
                           (range xmin (inc xmax))))
              (range ymin (inc ymax))))))

;;; Stuff that should be in clojure.core

(defn signum ^long [^double d]
  (long (Math/signum d)))

(defn sum [xs]
  (reduce + xs))

(defn transpose [mat]
  (apply mapv vector mat))

(defn elem [x xs]
  (some #{x} xs))

(defn permutations [[h & t :as coll]]
  (if (nil? t)
    [coll]
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn converge
  "Apply a function `f' until it converges."
  [f xs]
  (let [xss (f xs)]
    (if (= xss xs) xs (recur f xss))))

(defn converge-when
  "Apply a function `f' until it converges and count how long that takes."
  ([f xs]   (converge-when f xs 0))
  ([f xs n]
   (let [xss (f xs), m (inc n)]
     (if (= xss xs) [m xs] (recur f xss m)))))

(defn map-val
  "Map over the values of a given map."
  [f hmap]
  (into {} (map (fn [[k v]] {k (f v)})) hmap))

(defn filter-val
  "Filter a map by applying `f' to its values."
  ([f]      (filter #(f (val %))))
  ([f hmap] (filter #(f (val %)) hmap)))

(defn map-from-coll-with
  "Turn a collection into a map.  First map `g' over every element to
  produce a map and then merge the resulting maps, applying `f' in case
  of duplicate keys.  For example:

    (map-from-coll-with + #(hash-map % 1) [1 2 3 1]) ≡ {1 2, 3 1, 2 1}."
  [f g coll]
  (apply merge-with f (map g coll)))

(defn map-from-coll
  "Like `map-from-coll-with', but ignore the second value in case of a
  conflict."
  [g coll]
  (map-from-coll-with (fn [a _] a) g coll))

;;; Matrix manipulation

(defn mat-ix
  "Return the element at index (i, j).  Returns `nil' if index is
  out-of-bounds."
  [m [i j]]
  (let [rows (count m)
        cols (count (first m))]
    (when (and (< -1 i rows) (< -1 j cols))
      (nth (nth m i) j))))

(defn map-matrix
  "Map a function f(i, j, el) over all elements of a matrix with
  indices."
  [f mat]
  (apply concat
         (keep-indexed (fn [i row]
                         (keep-indexed (fn [j el]
                                         (f i j el))
                                       row))
                       mat)))

;;; Algorithms

(defn- dijkstra
  "Dijkstra's shortest path algorithm.

  The given `more' function is a function of two arguments that computes
  the neighbours and their costs for each index.  It gets the index to
  compute the neighbours for, as well as the already seen points.  This
  allows for filtering while computing the neighbours, which may improve
  performance."
  [start more]
  (letfn [(mincost [cost cost*]
            (fn [cc] (if cc          ; if value exists, take the minimum
                       (min cc (+ cost cost*))
                       (+ cost cost*))))]
    (loop [all-costs  {}
           seen       #{}
           looking-at (priority-map start 0)]
      (if (empty? looking-at)        ; no more points to look at -> STOP
        all-costs
        (let [[u cost-u] (peek looking-at)
              tail (pop looking-at)]
          (if (seen u)               ;  check for seen in case `more' doesn't
            (recur all-costs seen tail)
            (recur (assoc all-costs u cost-u) ; final, minimal, cost of a point
                   (conj seen u)
                   (reduce (fn [acc [n cost-n]]
                             ;; Remember only the minimal cost of a neighbour.
                             (update acc n (mincost cost-u cost-n)))
                           tail
                           (more u seen)))))))))
