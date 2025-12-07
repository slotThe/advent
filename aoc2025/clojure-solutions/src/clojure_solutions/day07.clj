(ns clojure-solutions.day07
  (:require [clojure.string :as s]
            [clojure-aoc-util.util :as u]
            [clojure-aoc-util.coords :as c])
  (:gen-class))

(defn- inside [[x y] [[a b] [c d]]]
  (and (< a x) (< x c) (< b y) (< y d)))

(defn- parse []
  (let [i (c/seq->map (s/split-lines (slurp "../inputs/day07.txt")))]
    {:bb (c/bounding-box i)
     :start (ffirst (u/filter-val (partial = \S) i))
     :splitters (into #{} (keys (u/filter-val (partial = \^) i)))}))

(defn- solve [inp]
  (let [{:keys [bb start splitters]} inp]
    (loop [bs {start 1}, hs {}, ss 0]   ; beams, hits, splits
      (if (empty? bs)
        [ss (u/sum (keep (fn [[[_ y] n]] (when (= y (second (second bb))) n)) hs))]
        (let [[bs hs ss]
              (reduce (fn [[bs hs ss] [b n]]
                        (let [m (merge-with + hs {b n}), neigh [{(c/left b) n} {(c/right b) n}]]
                          (cond
                            (not (inside b bb))     [bs                    m ss]
                            (contains? splitters b) [(apply conj bs neigh) m (inc ss)]
                            :else                   [(conj bs {b n})       m ss])))
                      [[] hs ss]
                      (map (fn [[b n]] [(c/below b) n]) bs))]
          (recur (apply merge-with + {} bs) hs ss))))))

(defn -main [& _]
  (let [[f s] (solve (parse))]
    (assert (and (= 1681 f) (= 422102272495018 s)))
    (u/print-day 7 f s)))
