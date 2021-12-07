(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day2 :as day2]
            [clojure-solutions.day3 :as day3]
            [clojure-solutions.day4 :as day4]
            [clojure-solutions.day5 :as day5]
            [clojure-solutions.day6 :as day6]
            [clojure-solutions.day7 :as day7]
            ))

(defn -main [& args]
  (println "Day Two:")
  (println (day2/one))
  (println (day2/two))

  (println "Day Three:")
  (println (day3/one))
  (println (day3/two))

  (println "Day Four:")
  (day4/day4)

  (println "Day Five:")
  (day5/day5)

  (println "Day Six:")
  (day6/day6)

  (println "Day Seven:")
  (day7/day7)
  )
