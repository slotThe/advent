(ns clojure-solutions.day24
  (:require [clojure.string :as str]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :refer [+ -]]
            [clojure-aoc-util.util :as aoc]))

(defn- parse []
  (->> (slurp "../inputs/day24.txt")
       str/split-lines
       (map (comp (partial map read-string)
                  (partial re-seq #"-*\d+")))))

(defn- pos [[x y z & _]]
  [x y z])

(defn- vel [[_ _ _ vˣ vʸ vᶻ]]
  [vˣ vʸ vᶻ])

(defn- intersect2d
  "Calculate the intersection of two 2-dimensional lines:

       vʸ·(x₂ - x) + vˣ·(x - x₂)
      ——————————————
           vˣ·v₂ʸ - vʸ·v₂ˣ"
  [[x y _ vˣ vʸ _] [x₂ y₂ _ v₂ˣ v₂ʸ _]]
  (let [denom (- (* vˣ v₂ʸ) (* vʸ v₂ˣ))]
    (when (not= 0 denom)
      (/ (+ (* vʸ (- x₂ x))
            (* vˣ (- y  y₂)))
         denom))))

(defn- solve1 [inp]
  (let [min 200000000000000, max 400000000000000]
    (aoc/sum
     (for [n (range (count inp)), m (range n (count inp))
           :let [l₁                       (nth inp n)
                 [pˣ pʸ _ vˣ vʸ _ :as l₂] (nth inp m)
                 intersection  (intersect2d l₁ l₂)
                 intersection' (intersect2d l₂ l₁)]
           :when (and intersection
                      (> intersection  0)
                      (> intersection' 0)
                      (<= min (+ pˣ (* vˣ intersection)) max)
                      (<= min (+ pʸ (* vʸ intersection)) max))]
       1))))

(defn- solve2
  "Oh boy.
  So, solving this part essentially boils down to solving

    cₚ + tᵢ·cᵥ = hₚ + tᵢ·hᵥ,

  for cₚ and cᵥ for all hailstones h, where cₚ ≔ (cˣ, cʸ, cᶻ) is the
  position, and cᵥ ≔ (cᵥˣ, cᵥʸ, cᵥᶻ) is the velocity we want to find.
  Rearranging the equation gives

    (cₚ - hₚ) = tᵢ·(hᵥ - cᵥ).

  Now comes the part where you actually need to think: What does it mean
  that these two vectors are the same, modulo some scalar? It means that
  they are, in fact, parallel. Hence,

    (cₚ - hₚ) × (hᵥ - cᵥ) = 0,                                       (1)

  where × is the cross product. There are six unknown variables, so we
  need a system of 6 equations to solve them. Randomly picking
  hailstones h₁, h₂, h₃, and multiplying out equation (1), we get

    cₚ × cᵥ - cₚ × h₁ᵥ - h₁ₚ × cᵥ = h₁ₚ × h₁ᵥ,
    cₚ × cᵥ - cₚ × h₂ᵥ - h₂ₚ × cᵥ = h₂ₚ × h₂ᵥ,
    cₚ × cᵥ - cₚ × h₃ᵥ - h₃ₚ × cᵥ = h₃ₚ × h₃ᵥ.

  The cₚ × cᵥ is annoying, but luckily it is in all three equations!
  That means we can reduce around a bit to—for example—get

    cₚ × (h₂ᵥ - h₁ᵥ) + (h₂ₚ - h₁ₚ) × cᵥ = h₁ₚ × h₁ᵥ - h₂ₚ × h₂ᵥ,
    cₚ × (h₃ᵥ - h₁ᵥ) + (h₃ₚ - h₁ₚ) × cᵥ = h₁ₚ × h₁ᵥ - h₃ₚ × h₃ᵥ.

  This is a system of 6 equations for 6 unknowns. Nice."
  [inp]
  (letfn [(cross-product-as-matrix [[x y z]]
            ;; The cross product a × b can also be written as a matrix product
            ;; A·b, where A is exactly the matrix below.
            [[0     (- z) y    ]
             [z     0     (- x)]
             [(- y) x     0    ]])
          (cross-of-vec [v]             ; vₚ × vᵥ
            (mat/cross (pos v) (vel v)))
          (cross-at [f v₁ v₂]           ; f(v₁) - f(v₂)
            (- (cross-product-as-matrix (f v₁))
               (cross-product-as-matrix (f v₂))))]
    (let [[h₁ h₂ h₃] (take 3 inp)
          rhs (vec (concat
                    (- (cross-of-vec h₁) (cross-of-vec h₂)) ; h₁ₚ × h₁ᵥ - h₂ₚ × h₂ᵥ
                    (- (cross-of-vec h₁) (cross-of-vec h₃)) ; h₁ₚ × h₁ᵥ - h₃ₚ × h₃ᵥ
                    ))
          A (vec (concat
                  (mapv (fn [a b] (vec (concat a b)))
                        (cross-at vel h₂ h₁)  ; h₂ᵥ - h₁ᵥ
                        (cross-at pos h₂ h₁)) ; h₂ₚ - h₁ₚ
                  (mapv (fn [a b] (vec (concat a b)))
                        (cross-at vel h₃ h₁) ; h₃ᵥ - h₁ᵥ
                        (cross-at pos h₃ h₁) ; h₃ₚ - h₁ₚ
                        )))]
      (->> (mat/mmul (mat/inverse A) rhs)
           (take 3)
           aoc/sum Math/ceil long))))

(defn day24 [p]
  (case p
    :one (solve1 (parse))
    :two (solve2 (parse))))
