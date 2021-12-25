(ns clojure-solutions.day19
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day19.txt")
       split-groups
       (map str/split-lines)
       (map (fn [[_ & coords]]
              (map #(read-string (str "[" % "]")) coords)))))

(defn- permute [xs]
  (letfn [(rotations [[x y z]]
            [[x    y     z]
             [x (- z)    y]
             [x (- y) (- z)]
             [x    z  (- y)]])
          (faces [[x y z]]
            [[x        y     z]
             [y     (- x)    z]
             [(- x) (- y)    z]
             [(- y)    x     z]
             [y        z     x]
             [y     (- z) (- x)]])]
    (transpose
     (map (fn [pt] (apply concat
                          (for [r (faces pt)]
                            (rotations r))))
          xs))))

(defn sub [[x y z] [x' y' z']]
  [(- x x') (- y y') (- z z')])

(defn add [[x y z] [x' y' z']]
  [(+ x x') (+ y y') (+ z z')])

(defn- overlap [xs ys]
  (->> (for [x xs, y ys] (sub x y))
       (map-from-coll-with + (fn [a] {a 1}))
       (filter-val #(>= % 12))
       (map first)))

(defn- match [xs ys]
  (for [perm-ys (permute ys)
        pos     (overlap xs perm-ys)]
    {:scanner pos
     :points  (map (partial add pos) perm-ys)}))

(defn- align [res [ref & refs] scanners]
  (if (empty? scanners)
    res
    (let [{found true, not-found false}
          (group-by (comp some? :found)
                    (for [scanner scanners
                          :let [ali (match ref scanner)]]
                      (if (empty? ali)
                        {:not-found scanner}
                        {:found (first ali)})))]
      (recur (concat (map :found found) res)
             (concat (map :points (map :found found)) refs)
             (map :not-found not-found)))))

(defn manhattan [[x y z] [x* y* z*]]
  (+ (+ (Math/abs (- x x*))
        (Math/abs (- y y*)))
     (Math/abs (- z z*))))

(defn day19 []
  (let [input   (parse)
        initial (first input)
        beacons (rest  input)
        work (align [{:scanner [0 0 0], :points initial}]
                    [initial]
                    beacons)]
    (println (->> work                  ; => 454
                  (map :points)
                  (apply concat)
                  (into #{})
                  count))
    (println (apply max                 ; => 10813
                    (for [p (map :scanner work)
                          q (map :scanner work)]
                      (manhattan p q))))))
