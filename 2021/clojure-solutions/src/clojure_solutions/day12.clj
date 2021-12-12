(ns clojure-solutions.day12
  (:require [clojure.string :as str]
            [clojure.set    :as set])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day12.txt")
       str/split-lines
       (map #(str/split % #"-"))
       (map (fn [[k v]]                 ; paths are bidirectional
              (into {}
                    (filter-val #(not (elem :start %)))
                    {(keyword k) #{(keyword v)}
                     (keyword v) #{(keyword k)}})))
       (apply merge-with set/union)))

(defn- small-cave? [kw]
  (= (name kw) (str/lower-case (name kw))))

(defn- small-cave-twice
  "Return the first small cave that appears twice in the list, if any.
  The consistency assumption here is that we will never have several
  small caves appear twice."
  [path]
  (let [small-path (filter small-cave? path)]
    (ffirst (filter-val #(= 2 %) (frequencies small-path)))))

(defn- solve [keep connections]
  (letfn ((go [path kw]
              (let [path* (conj path kw) ; new path
                    ;; Places we can visit
                    downs (filter #(keep path* %) (connections kw))]
                (if (= kw :end)
                  [path*]
                  (mapcat #(go path* %) downs)))))
    (->> (connections :start)
         (mapcat #(go [:start] %))
         count)))

;; => 4754
(defn- part1 [xs]
  (solve (fn [path x] (not (and (small-cave? x) (elem x path))))
         xs))

;; => 143562
(defn- part2 [xs]
  (solve (fn [path x]
           ;; If a small cave already appears twice, make sure it
           ;; doesn't happen again.  Otherwise, just let everything
           ;; through.
           (not (and (small-cave-twice path)
                     (and (small-cave? x)
                          (elem x path)))))
         xs))

(defn day12 []
  (let [connections (parse)]
    (println (part1 connections))
    (println (part2 connections))))
