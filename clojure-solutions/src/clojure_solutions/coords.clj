(ns clojure-solutions.coords
  (:require [clojure.string :as str]
            [clojure-solutions.util :as util]))

;; 2d (and more, if applicable) coordinates
;;
;; x grows to the *right* and y grows *down*.

(defn below [[x y]] [x (inc y)])
(defn above [[x y]] [x (dec y)])
(defn right [[x y]] [(inc x) y])
(defn left  [[x y]] [(dec x) y])

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn sub [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn seq->map
  ([xs]
   (into {} (util/map-matrix (fn [i j el] [[j i] el]) xs)))
  ([xf xs]
   (into {} xf (util/map-matrix (fn [i j el] [[j i] el]) xs))))

(defn move [kw pt]
  (case kw
    :NW (above (right pt))
    :N  (above pt)
    :NE (above (left pt))
    :W  (left pt)
    :E  (right pt)
    :SW (below (left pt))
    :S  (below pt)
    :SE (below (right pt))))

(defn bounding-box
  "Calculate the bounding box for the given sequence."
  [xs]
  (let [xs' (if (map? xs) (keys xs) xs)]
    (letfn [(go [op]
                (reduce (partial mapv op) xs'))]
      [(map dec (go min)) (map inc (go max))])))

(defn dimensions [coords]
  (second (bounding-box coords)))

(defn dir->kw [dir]
  (case dir
    [1  0] :E
    [-1 0] :W
    [0 -1] :N
    [0  1] :S))

(defn kw->dir [kw]
  (case kw
    :E [1  0]
    :W [-1 0]
    :N [0 -1]
    :S [0  1]))

(defn neighbours4
  ([pt] [(above pt) (left pt) (right pt) (below pt)])
  ([pt cols rows]
   (filter (fn [[c r]]
             (and (< -1 c cols)
                  (< -1 r rows)))
           (neighbours4 pt))))

(defn neighbours8 [pt]
  [(above (left pt)) (above pt) (above (right pt))
          (left pt)                    (right pt)
   (below (left pt)) (below pt) (below (right pt))])
