(ns clojure-aoc-util.coords
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as util]))

(import (clojure.lang MapEntry))

;; 2d (and more, if applicable) coordinates
;;
;; x grows to the *right* and y grows *down*.

(def north [0 -1])
(def south [0  1])
(def west  [-1 0])
(def east  [1  0])

(defn below [[x y]] [x (inc y)])
(defn above [[x y]] [x (dec y)])
(defn right [[x y]] [(inc x) y])
(defn left  [[x y]] [(dec x) y])

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn sub [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn taxicab
  "The taxicab (L¹) norm of two points."
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn seq->map
  "Turn a given sequence into a coordinate map.
  Optionally, takes a transducer that operates on map entries and
  forwards it to 'into'."
  ([xs]    (into {}    (util/map-matrix (fn [i j el] (MapEntry. [j i] el)) xs)))
  ([xf xs] (into {} xf (util/map-matrix (fn [i j el] (MapEntry. [j i] el)) xs))))

(defn turn
  "Turn left or right."
  [dir looking]
  (let [dirs [north west south east]
        pos  (.indexOf dirs looking)]
    (case dir
      :L (nth dirs (mod (inc pos) 4))
      :R (nth dirs (mod (dec pos) 4)))))

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
