(ns clojure-solutions.day16
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (let [valves (->> (slurp "../inputs/day16.txt")
                    str/split-lines
                    (map (partial re-seq #"[A-Z]{2}|\d+"))
                    (map (fn [[v i & vs]]
                           [v (read-string i) vs])))]
    {:flows  (into {} (map (fn [[v i _ ]] [v i ])) valves)
     :routes (into {} (map (fn [[v _ vs]] [v vs])) valves)}))

(defn- move [{:keys [flows routes]} time [[valve open] points]]
  (concat
   (map (fn [a] [[a open] points]) (get routes valve))
   (when-not (or (get open valve) (= (get flows valve) 0))
     [[[valve (conj open valve)]
       (+ (* (dec time) (get flows valve)) points)]])))

(defn- get-max [xs]
  (map-from-coll-with max (fn [[a b]] {a b}) xs))

(defn- go [ms positions time]
  (loop [t time, pss positions]
    (if (= 0 t)
      (get-max (map (fn [[a b]] [(:open a) b]) pss))
      (recur (dec t)
             (get-max (mapcat (partial move ms t) pss))))))

(defn- solve [ms t]
  (go ms [[["AA" #{}] 0]] t))

(defn day16 [p]
  (let [inp (parse)]
    (case p
      :one (val (apply max-key val (solve inp 30)))
      :two (let [solve2 (solve inp 26)]
             (apply max
                    (for [[me m] solve2, [el e] solve2
                          :when (empty? (set/intersection me el))]
                      (+ m e)))))))
