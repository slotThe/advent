(ns clojure-solutions.day7
  (:require [clojure.string :as str])
  (:use [clojure-aoc-util.util] :reload))

(defn- parse []
  (letfn [(dir-or-file [[type name]]
            (if (= type "dir")
              name
              (read-string type)))]
    (->> (slurp "../inputs/day7.txt")
         (#(str/split % #"\$ "))
         rest
         (map (comp (partial map words) str/split-lines))
         (map (fn [[cmd & inp]]
                {:command [(keyword (first cmd)) (second cmd)]
                 :output (map dir-or-file inp)})))))

(defn- simulate [cmds]
  (loop [pwd  []
         tree {}
         [{output :output, [cmd dir] :command} & cs] cmds]
    (if (nil? cmd)
      tree
      (case cmd
        :cd (if (= dir "..")
              (recur (pop pwd) tree cs)
              (recur (conj pwd dir) tree cs))
        :ls (recur pwd (assoc tree pwd output) cs)))))

(defn- solve []
  (->> (simulate (parse))
       (sort-by (comp count first) >)
       (reduce (fn [hm [path vals]]
                  (assoc hm
                         path
                         (reduce (fn [acc el]
                                   (+ acc (if (string? el)
                                            (get hm (conj path el))
                                            el)))
                                 0
                                 vals)))
               {})
       (into {})))

(defn day7 [p]
  (let [hm (solve)]
    (case p
      :one (reduce (fn [acc [_ el]]
                     (if (<= el 100000) (+ acc el) acc))
                   0
                   hm)
      :two (let [need (- 30000000 (- 70000000 (get hm ["/"])))]
             (val (apply min-key val (filter-val #(<= need %) hm)))))))
