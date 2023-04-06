(ns clojure-solutions.day24
  (:use [clojure-solutions.util] :reload))

(def round
  "A single round of the MONAD."
  #"inp w
mul x 0
add x z
mod x 26
div z (-*\d+)
add x (-*\d+)
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y (-*\d+)
mul y x
add z y")

(defn- parse []
  (->> (slurp "./input/day24.txt")
       (re-seq round)
       (map (fn [[_ div-z add-x add-y]]
              (mapv read-string [div-z add-x add-y])))))

(defn- step
  "What a single step of the MONAD actually does."
  [w z div-z add-x add-y]
  (let [new-z (quot z div-z)]
    (if (not= w (+ (mod z 26) add-x))
      (+ (* new-z 26) (+ w add-y))
      new-z)))

(def find-num
  (memoize
   (fn [guesses z [cur & steps]]
     (cond (and (nil? cur) (= z 0)) '(())
           (nil? cur)               '()
           :else
           (let [[div-z add-x add-y] cur]
             (for [w   guesses
                   rst (find-num guesses
                                 (step w z div-z add-x add-y)
                                 steps)]
               (conj rst w)))))))

(defn day24 []
  (let [input (parse)
        solve (fn [guesses]
                (->> input
                     (find-num guesses 0)
                     first
                     (coll-to-base 10)))]
    (println (solve (range 9 0 -1)))    ; => 93499629698999
    (println (solve (range 1 10)))      ; => 11164118121471
    ))
