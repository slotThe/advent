(ns clojure-solutions.day17
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day17.txt")
       (re-find #"target area: x=(\w+)..(\w+), y=(-\w+)..(-\w+)")
       (map read-string)
       ((fn [[_ x1 x2 y1 y2]]
          {:x [x1 x2]
           :y [y1 y2]}))))

(defn- in-target-area [{[x1 x2] :x, [y1 y2] :y} [p1 p2]]
  (and (<= x1 p1 x2) (<= y1 p2 y2)))

(defn- behind-target-area
  "A point is behind the target area when its x-position is greater than
  the greatest x-position of the area or its y-position is smaller than
  the (smallest) y-position of the area.  Consistency assumptions:
    - x1 < x2 and both are positive,
    - y1 > y2 and both are negative."
  [{[_ x2] :x, [y1 _] :y} [p1 p2]]
  (or (> p1 x2) (< p2 y1)))

(defn- simulate [vels target]
  (loop [[x y]         [0 0]            ; start at the origin
         [x-vel y-vel] vels
         max-y         0]
    (let [new-x    (+ x x-vel)
          new-y    (+ y y-vel)
          new-xvel (- x-vel (compare x-vel 0)) ; drag
          new-yvel (dec y-vel)]                ; gravity
      (cond (behind-target-area target [x y]) :failure
            (in-target-area     target [x y]) {:max-y max-y}
            :else (recur [new-x new-y] [new-xvel new-yvel] (max new-y max-y))))))

(defn day17 []
  (let [{[_ x2] :x, [y1 _] :y} (parse)
        max-height (quot (* (dec (- y1)) (- y1)) 2) ; See Note [Max Height]
        successes (filter :max-y
                          (for [x-vel (range 0  (inc x2))
                                y-vel (range y1 (- y1))]
                            (simulate [x-vel y-vel] (parse))))]
    (println max-height)                ; 6555
    (println (count successes))         ; => 4973
    ))

;; Note [Max Height]
;;
;; This is using the fact that y₁ is negative (and smaller than y₂) to
;; directly calculate the maximal height using the theoretical max
;; initial velocity, which is pred(-y₁) (under the assumption that there
;; exists an x velocity such that we don't miss).  The point is that,
;; when climbing in the y direction, we just always decrease the
;; velocity by 1 until we hit y-velocity ≡ 0 at the highest point.  We
;; then stay at the highest spot for one step and, when the y-velocity
;; gets decreased to -1, start moving again.  After that we go down in
;; the same fashion, meaning when will depart from (x, -y₁) with a
;; velocity of succ(y₁) and from (x', 0) with one of exactly y₁.  This
;; puts us into the last allowed row after that jump.  This is also why
;; we can't use -y₁ as an initial velocity; we'd jump succ(y₁) fields
;; from 0 and miss!
