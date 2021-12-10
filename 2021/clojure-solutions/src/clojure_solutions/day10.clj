(ns clojure-solutions.day10
  (:require [clojure.string :as str]))

(defn- valid?
  "Check if a given sequence of parentheses is valid; if not, return
  either the error or parens that are still needed.  The algorithm is
  basically
    ([](} -> nil ([](} -> ) [](} -> ]) ](} -> ) (} -> )) } -> :fail    }
    ([](  -> nil ([](  -> ) [](  -> ]) ](  -> ) (  -> ))   -> :partial ))"
  [cs]
  (letfn ((go [[c & cs :as closing] [x & xs :as input]]
              (let [closing-x ({\{ \}, \[ \], \( \), \< \>} x)]
                (cond
                  ;; Group ends -> remove both parens.
                  (= c x)           (go cs xs)
                  ;; new group begins -> add the closing delimiter to `closing'.
                  (some? closing-x) (go (cons closing-x closing) xs)
                  ;; No group began and no group ended -> wrong closing paren.
                  (some? x)         {:fail x}
                  ;; End of string -> return partial results
                  (nil? input)      {:partial closing}))))
    (go [] cs)))

;; => 413733
(defn- part1 [m]
  (let [score {\) 3, \] 57, \} 1197, \> 25137}]
    (->> m
         (filter (fn [{:keys [fail]}] (some? fail)))
         (mapcat vals)
         (map score)
         sum)))

;; => 3354640192
(defn- part2 [m]
  (let [p-score {\) 1, \] 2, \} 3, \> 4}
        score #(reduce (fn [acc x] (+ (* acc 5) (p-score x))) 0 %)]
    (->> m
         (filter (fn [{:keys [partial]}] (some? partial)))
         (mapcat vals)
         (map score)
         sort
         (#(nth % (quot (count %) 2))))))

(defn day10 []
  (let [parses (map valid? (str/split-lines (slurp "./input/day10.txt")))]
    (println (part1 parses))
    (println (part2 parses))))
