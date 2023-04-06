(ns clojure-solutions.day21
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day21.txt")
       (re-seq #"Player \d starting position: (\d+)")
       (map (comp read-string second))))

(defn- move
  "Move forward by `n` steps."
  [n]
  (inc (mod (dec n) 10)))

(defn- part1 [[p1 p2] target]
  (letfn [(inc-die [x]
            (if (= x (dec target))
              [target 1 2]
              [(inc x) (+ 2 x) (+ 3 x)]))]
    (loop [[pos score] [p1 0]
           next-turn   [p2 0]
           rolls       0
           [x1 x2 x3]  [1 2 3]]
      (let [new-pos   (move (+ pos x1 x2 x3)) ; move forward
            new-score (+ new-pos score)]
        (if (<= target new-score)
          (* (second next-turn) (+ rolls 3))  ; score of loser * #rolls
          (recur next-turn [new-pos new-score] (+ rolls 3) (inc-die x3)))))))

(def part2
  (let [freqs (frequencies (for [x [1 2 3], y [1 2 3], z [1 2 3]]
                             (+ x y z)))]
    (memoize
     (fn [[turn score] next-turn]
       (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)])     ; add up results
               [0 0]
               (for [[roll multiplier] freqs
                     :let [new-turn (move (+ turn roll))
                           new-score (+ score new-turn)]]
                 (if (>= new-score 21)     ; check if current turns wins
                   [multiplier 0]
                   ;; Otherwise, check the next turn and flip the
                   ;; result (it's `next-turn`s turn, next turn :)
                   ((fn [[next this]]
                      [(* multiplier this) (* multiplier next)])
                    ;; Call `part2` to benefit from the memoisation.
                    (part2 next-turn [new-turn new-score])))))))))

(defn day21 []
  (let [[p1 p2] (parse)]
    (println (part1 [p1 p2] 1000))              ; => 556206
    (println (apply max (part2 [p1 0] [p2 0]))) ; => 630797200227453
    ))
