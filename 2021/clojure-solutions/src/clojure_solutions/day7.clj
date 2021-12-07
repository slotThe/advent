(ns clojure-solutions.day7
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (map read-string (str/split (slurp "./input/day7.txt") #",")))

(defn- median [xs]
  (let [xs' (sort xs), n (count xs), q (quot n 2)]
    (if (odd? n)
      (nth xs' q)
      (quot (+ (nth xs' q)
               (nth xs' (dec q)))
            2))))

(defn- avg+-1 [xs]
  (let [n (/ (sum xs) (count xs))]
    (map int [(Math/ceil n) (Math/floor n)])))

(defn- difference [a b]
  (if (> a b) (- a b) (- b a)))

(defn- solve [f xs]
  (sum (map f xs)))

(defn day7 []
  (let [xs (parse)
        [a- a+] (avg+-1 xs)
        part2 (fn [a x]
                (let [n (difference a x)]
                  (quot (* n (inc n)) 2)))] ; Gauß <3
    ;; part 1 => 328187
    (println (solve #(difference (median xs) %) xs))
    ;; part 2 => 91257582
    (println (min (solve #(part2 a- %) xs) (solve #(part2 a+ %) xs)))))
