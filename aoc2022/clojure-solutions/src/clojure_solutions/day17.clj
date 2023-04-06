(ns clojure-solutions.day17
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(def jets
  (vec (map #(case %
               \> [ 1 0]
               \< [-1 0])
            (str/trim (slurp "../inputs/day17.txt")))))

(def pieces
  "Pieces with their associated height."
  [[[[0 0] [1 0] [2 0] [3 0]] 0]
   [[[1 0] [0 -1] [1 -1] [2 -1] [1 -2]] 2]
   [[[2 0] [2 -1] [0 -2] [1 -2] [2 -2]] 2]
   [[[0 0] [0 -1] [0 -2] [0 -3]] 3]
   [[[0 0] [0 -1] [1 0] [1 -1]] 1]])

(defn- neighbours [rocks [x y] seen]
  (filter (fn [[a _ :as p]]
            (and (<= 0 a 6)
                 (not (contains? rocks p))
                 (not (contains? seen  p))))
          ;; We only need to move downwards
          [[x (dec y)] [(inc x) y] [(dec x) y]]))

(defn- move [xys [cx cy]]
  (map (fn [[x y]]
         [(+ cx x) (+ cy y)])
       xys))

(defn- crashed? [xys grid]
  (some (fn [[x y]]
          (or (not (<= 0 x 6))
              (contains? grid [x y])))
        xys))

(defn- reachable?
  "Is the point reachable from the air?  This is true iff it an air block
  has a north, east, or west neighbour."
  [air [x y]]
  (some (partial contains? air) [[x (inc y)] [(inc x) y] [(dec x) y]]))

(defrecord State [jet-ix piece-ix rocks])

(defn- fall
  "Given a 'State', produce another 'State' that represents a single piece
  having fallen to completion."
  [{:keys [jet-ix piece-ix rocks]}]
  (let [floor-height (reduce max (map second rocks))
        [p h] (nth pieces piece-ix)
        start-y (+ 4 h floor-height)]
    (loop [jet-pos jet-ix
           pos (move p [2 start-y])]
      (let [after-jet* (move pos (nth jets (mod jet-pos (count jets))))
            after-jet (if (crashed? after-jet* rocks) pos after-jet*)
            after-fall (move after-jet [0 -1])]
        (if (crashed? after-fall rocks)
          (let [rocks* (apply conj rocks after-jet) ; update rocks with newly fallen piece
                air (into #{} (flood-fill [0 start-y] (partial neighbours rocks*)))]
            (->State (mod (inc jet-pos) (count jets))
                     (mod (inc piece-ix) 5)
                     (into #{}          ; prune playing field
                           (filter (partial reachable? air))
                           rocks*)))
          (recur (inc jet-pos) after-fall))))))

(defn- find-cycle [list]
  (loop [[x & xs] list, i 0, seen {}]
    (if-let [j (get seen x)]
      [j i]
      (recur xs (inc i) (assoc seen x i)))))

(defn day17 [p]
  (let [rounds (iterate fall (->State 0 0 #{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0]}))]
    (letfn [(height-of [grid]
              (second (apply max-key second grid)))
            (height-at [ix]
              (height-of (:rocks (nth rounds ix))))]
      (case p
        :one (height-at 2022)
        :two (let [rocks 1000000000000
                   [start end] (find-cycle
                                (for [r rounds, :let [grid (:rocks r)]]
                                  ;; Normalise the grid by letting it start at 0.
                                  (assoc r :rocks
                                         (into #{} (move grid [0 (- (height-of grid))])))))
                   iterations (dec (quot (- rocks start) (- end start)))
                   extra-bit       (mod  (- rocks start) (- end start))]
               (+ (height-at (+ end extra-bit))            ; bit that doesn't fit in cycle
                  (* iterations
                     (- (height-at end) (height-at start)) ; cycle-height
                     )))))))
