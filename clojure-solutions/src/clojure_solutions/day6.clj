(ns clojure-solutions.day6)

(defn- solve [n]
  (->> (slurp "./input/day6.txt")
       (partition n 1)
       (take-while #(not= (count (into #{} %)) (count %)))
       count
       (+ n)))

(defn day6 [part]
  (case part
    :one (solve 4)
    :two (solve 14)))
