(ns clojure-solutions.day14
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day14.txt")
       split-groups
       ((fn [[sample polymers]]
          {:sample sample
           :polymers (map-from-coll
                      #(let [[[_ a b]] (re-seq #"(\w+) -> (\w)" %)]
                         {(vec a) (first (vec b))})
                      (str/split-lines polymers))}))))

(defn- simulate
  "Simulate polymer construction.  Returns every step at once as an
  infinite list."
  [sample polymers]
  (iterate
   #(reduce (fn [acc [[a c :as k] v]]
              (let [b (polymers k)
                    ;; Update the value if it exists, insert otherwise
                    updater (fn [x] (if (nil? x) v (+ x v)))]
                ;; We are assuming that `b' is always non-nil; i.e.,
                ;; that there is always a rule for `k' (otherwise, we
                ;; wouldn't even know what to do).
                (update (update acc [a b] updater)
                        [b c]
                        updater)))
            {}
            %)
   ;; Create frequency map of pairs as a starting point.
   (map-from-coll-with + #(hash-map % 1) (partition 2 1 sample))))

(defn- solve [step]
  (->> step
       (map-from-coll-with + (fn [[[a b] v]]
                               (if (= a b) {a (* 2 v)} {a v, b v})))
       vals
       (map #(quot (inc %) 2))          ; we counted everything twice
       (#(- (apply max %) (apply min %)))))

(defn day14 []
  (let [{:keys [sample polymers]} (parse)
        list (simulate sample polymers)]
    (println (solve (nth list 10)))     ; => 2584
    (println (solve (nth list 40)))     ; => 3816397135460
    ))
