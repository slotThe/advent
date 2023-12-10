(ns clojure-aoc-util.util
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

;;; Pretty printing

(defn print-day [day one two]
  (println "!!! Day" day "!!!")
  (println "First  Task:" one)
  (println "Second Task:" two)
  (println))

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

;;; Matrix manipulation

(defn mat-ix
  "Return the element at index (i, j).  Returns `nil' if index is
  out-of-bounds."
  [m [i j]]
  (nth (nth m i nil) j nil))

(defn map-over-matrix
  "Map a function f(i, j, el) over all elements of a matrix with
  indices."
  [f mat]
  (keep-indexed (fn [i row]
                  (keep-indexed (fn [j el]
                                  (f i j el))
                                row))
                mat))

(defn map-matrix
  "Map a function f(i, j, el) over all elements of a matrix with
  indices, and concatenate the result into a 1-dimensional vector."
  [f mat]
  (apply concat (map-over-matrix f mat)))

;;; Random util

(defn not-in? [coll k]
  (not (contains? coll k)))

(defn signum ^long [^double d]
  (long (Math/signum d)))

(defn sum [xs]
  (reduce + xs))

(defn transpose [mat]
  (apply mapv vector mat))

(defn elem [x xs]
  (some #{x} xs))

(defn permutations [[_ & t :as coll]]
  (if (nil? t)
    [coll]
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn tails [xs]
  (reductions (fn [x _] (rest x)) xs xs))

(defn converge
  "Apply a function `f' until it converges."
  [f xs]
  (let [xss (f xs)]
    (if (= xss xs) xs (recur f xss))))

(defn converge-by
  "Apply the function `f' until it converges.
  Before doing so, apply the function `g' to the previous and current
  result.  Like `converge-when', count how long that takes."
  ([g f xs]   (converge-by g f xs 0))
  ([g f xs n]
   (let [xss (f xs), m (inc n)]
     (if (= (g xss) (g xs)) [m xs] (recur g f xss m)))))

(defn converge-when
  "Apply a function `f' until it converges and count how long that takes."
  [f xs]
  (converge-by identity f xs 0))

(defn map-val
  "Map over the values of a given map."
  [f hmap]
  (into {} (map (fn [[k v]] {k (f v)})) hmap))

(defn filter-val
  "Filter a map by applying `f' to its values.
  Returns a transducer if no collection is provided."
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

;;; Algorithms

(defn dijkstra
  "Dijkstra's shortest path algorithm.

  `start' is either a single, or a sequence of starting positions.  The
  given `more' function is a function of two arguments that computes the
  neighbours and their costs for each index.  It gets the index to
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
           looking-at (if (seq? start)
                        (apply priority-map (mapcat vector start (repeat 0)))
                        (priority-map start 0))]
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

(defn flood-fill
  "A simple flood-fill.
  The argument `start' is the starting position and `more' is a function
  taking two arguments: the position to find neighbours for, and the set
  of already visited nodes."
  [start more]
  (loop [remaining [start]
         seen #{}]
    (if (empty? remaining)
      seen
      (let [looking-at (peek remaining)]
        (recur (apply conj (pop remaining) (more looking-at seen))
               (conj seen looking-at))))))

(defn binary-search
  "https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/"
  ([mid pred lo hi]
   (loop [l lo, r hi]
     (if-let [m (mid l r)]
       (if (pred m)
         (recur l m)
         (recur m r))
       [l r])))
  ([pred ^long l ^long r]
   (binary-search #(when (> (- %2 %1) 1)
                     (quot (+ %1 %2) 2))
                  pred l r)))

(defn gcd
  "Compute the greatest common divisor of two numbers."
  [a b]
  (loop [a (abs a) b (abs b)] ; Euclidean algorithm
    (if (zero? b) a (recur b (mod a b)))))

(defn lcm
  "Compute the least common multiple of two positive numbers."
  [a b]
  (* b (quot a (gcd a b))))
