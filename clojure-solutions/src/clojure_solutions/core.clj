(ns clojure-solutions.core
  (:gen-class)
  (:require [clojure-solutions.day1 :as day1]))

(defn -main [& args]
  (print-day 1 (day1/get-nth-most-wanted 1) (day1/get-nth-most-wanted 3)))

(defn- print-day [day one two]
  (println "!!! Day" 1 "!!!")
  (println "First  Task:" one)
  (println "Second Task:" two)
  (println))
