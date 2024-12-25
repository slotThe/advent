(ns clojure-solutions.day25
  (:require [clojure-aoc-util.util :as util]
            [clojure.string :as str])
  (:gen-class))

(defn- parse []
  (letfn [(to-vec [fix]
            (fn [xs]
              (mapv #(count (filter (partial = \#) %))
                    (util/transpose (fix xs)))))]
    (let [{locks true keys false}
          (->> (slurp "../inputs/day25.txt")
               (util/split-groups)
               (map str/split-lines)
               (group-by #(= \# (ffirst %))))]
      {:keys  (mapv (to-vec drop-last)        keys)
       :locks (mapv (to-vec (partial drop 1)) locks)})))

(defn -main [& _args]
  (let [{:keys [keys locks]} (parse)
        one (reduce + (for [k keys, l locks :when (every? #(<= % 5) (map + k l))] 1))
        _ (assert (= one 3287))]
    (util/print-day 25 one "Merry Christmas!")))
