(ns clojure-solutions.day22
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (let [pnums "(-*\\d+)..(-*\\d+)"
        match-cube (str "(on|off) x=" pnums ",y=" pnums ",z=" pnums)]
    (->> (slurp "./input/day22.txt")
         str/split-lines
         (map (partial re-find (re-pattern match-cube)))
         (map (fn [[_ on-off x1 x2 y1 y2 z1 z2]]
                [(keyword on-off)
                 (mapv read-string [x1 x2 y1 y2 z1 z2])])))))

(defn- intersect
  "Calculate the intersection of two cuboids.  Note that this is _always_
  a cuboid again."
  [[x11 x12 y11 y12 z11 z12] [x21 x22 y21 y22 z21 z22]]
  (let [intersect? (and (<= (max x11 x21) (min x12 x22))
                        (<= (max y11 y21) (min y12 y22))
                        (<= (max z11 z21) (min z12 z22)))]
    (when intersect?
      [(max x11 x21) (min x12 x22)
       (max y11 y21) (min y12 y22)
       (max z11 z21) (min z12 z22)])))

(defn- include-exclude
  "The main attraction—the inclusion-exclusion principle:

      |A ∪ B ∪ C| = |A| + |B| + |C| - |A ∩ B| - |A ∩ C| - |B ∩ C| + |A ∩ B ∩ C|.

  This generalises to higher `n` as expected.  The main insight for the
  implementation is that we can immediately see when the |A ∩ B ∩ C|
  part occurs by keeping track of the `excludes` (all subtractions)
  separately from the `includes` (all additions) and just add an
  intersection of an existing `exclude` as an `include` and vice versa."
  [{:keys [includes excludes]} [on-off cuboid]]
  {:includes ((if (= :on on-off)
                #(conj % cuboid)        ; `on` cuboids count for themselves
                identity)
              (concat includes (keep #(intersect cuboid %) excludes)))
   :excludes (concat excludes (keep #(intersect cuboid %) includes))})

(defn- solve [cubes]
  (letfn [(lit-up [{:keys [includes excludes]}]
            (letfn [(vol [[x1 x2 y1 y2 z1 z2]]
                      (* (inc (- x2 x1))
                         (inc (- y2 y1))
                         (inc (- z2 z1))))]
              (- (sum (map vol includes))
                 (sum (map vol excludes)))))]
    (->> cubes
         (reduce include-exclude
                 {:includes [] :excludes []})
         lit-up)))

(defn day22 []
  (let [input (parse)
        restrict (keep (fn [[on-off cuboid]]
                         (when-let [isect (intersect [-50 50 -50 50 -50 50] cuboid)]
                           [on-off isect]))
                       input)]
    (println (solve restrict))          ; => 581108
    (println (solve input))             ; => 1325473814582641
    ))
