(ns clojure-solutions.day3
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day3.txt")
       str/split-lines
       (map #(str-to-coll-base 2 %))))

(defn- freqs [xs compare]
  (let [n (quot (count xs) 2)]
    (->> xs
         (apply mapv vector)            ; transpose
         (map #(if (compare (sum %) n) 1 0)))))

;; 3959450
(defn one []
  (let [nums (parse)
        n (/ (count nums) 2)
        gamma (freqs nums >)
        epsilon (freqs [gamma] =)]      ; funky negation :^)
    (* (coll-to-base 2 gamma) (coll-to-base 2 epsilon))))

(defn- nth-freq [xs compare ix]
  (let [n (quot (count xs) 2)
        freq-sum (sum (map #(nth % ix) xs))]
    (if (compare freq-sum n) 1 0)))

(defn- get-rating [compare]
  (loop [nums (parse)
         ix 0]
    (let [n (nth-freq nums compare ix)
          xs (filter #(= n (nth % ix)) nums)]
      (if (= 1 (count xs))
        (first xs)
        (recur xs (inc ix))))))

;; 7440311
(defn two []
  (* (coll-to-base 2 (get-rating >=))
     (coll-to-base 2 (get-rating <))))
