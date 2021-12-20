(ns clojure-solutions.day18
  (:require [clojure.string :as str]
            [clojure.zip    :as z])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (map read-string (str/split-lines (slurp "./input/day18.txt"))))

(defn- go-up
  "Return a zipper at the root node."
  [zipper]
  (z/vector-zip (z/root zipper)))

(defn- find-node [pred zipper]
  (loop [z zipper]
    (cond (z/end? z) nil
          (pred   z) z
          :else      (recur (z/next z)))))

(defn- next-leaf [zipper]
  (find-node (comp int? z/node) (z/next zipper)))

(defn- prev-leaf [zipper]
  (when-let [z1 (z/prev zipper)]
    (if (int? (z/node z1)) z1 (recur z1))))

(defn- explode [zipper]
  (if-let [explode?
           (find-node (fn [z] (and (= 4 (count (z/path z))) ; deep enough?
                                   (vector? (z/node z))))   ; not a number?
                      zipper)]
    ;; We have some exploding to do.
    (let [[l r] (z/node explode?)          ; values for exploding pair
          z1 (z/replace explode? 0)        ; zero them
          z2 (if-let [zl (prev-leaf z1)]   ; fix lefts
               (next-leaf (z/edit zl + l))
               z1)]
      (if-let [zr (next-leaf z2)]          ; fix rights
        (prev-leaf (z/edit zr + r))
        z2))
    ;; Else, return the zipper at the root.
    (go-up zipper)))

(defn- split [zipper]
  (if-let [split?
           (find-node (fn [z] (and (int? (z/node z))  ; a number
                                   (> (z/node z) 9))) ; large enough
                      zipper)]
    ;; We have some splitting to do
    (z/edit split?
            (fn [x] [(int (/ x 2)) (int (Math/ceil (/ x 2)))]))
    ;; Else, return the zipper at the root.
    (go-up zipper)))

(defn- reduce-fish [zipper]
  ;; `converge' does not work with zippers for some reason :/
  (let [exp (explode zipper)]
    (if (not= exp zipper)
      (recur exp)
      (let [spl (split zipper)]
        (if (not= spl zipper)
          (recur spl)
          (z/root zipper))))))

(defn- magnitude [x]
  (if (vector? x)
    (+ (* 3 (magnitude (first  x)))
       (* 2 (magnitude (second x))))
    x))

(defn day18 []
  (let [prep-fish (comp reduce-fish z/vector-zip vector)
        input     (parse)]
    (println (magnitude (reduce prep-fish input))) ; => 4072
    (println (apply max                            ; => 4483
                    (for [x input, y input]
                      (magnitude (prep-fish x y)))))))
