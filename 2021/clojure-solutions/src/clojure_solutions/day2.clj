(ns clojure-solutions.day2
  (:require [clojure.string :as str]))

(defn- parse []
  (->> (slurp "./input/day2.txt")
       str/split-lines
       (map (fn [s]
              (let [[kw n] (str/split s #" ")
                    n (read-string n) ; something like ViewPatterns would be neat
                    kw (keyword kw)]
                (if (= :up kw) [:down (- n)] [kw n]))))))

;; 1524750
(defn one []
  (->> (parse)
       (reduce (fn [acc [k v]]
                 (update acc k #(+ v %)))
               {:forward 0 :down 0})
       ((fn [{f :forward d :down}]
          (* f d)))))

;; 1592426537
(defn two []
  (->> (parse)
       (reduce (fn [[aim depth horiz] [d i]]
                 (case d
                   :down    [(+ aim i) depth horiz]
                   :forward [aim (+ depth (* aim i)) (+ horiz i)]))
               [0 0 0])
       rest                             ; only depth and horiz
       (reduce *)))
