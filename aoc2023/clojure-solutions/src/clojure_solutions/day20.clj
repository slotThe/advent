(ns clojure-solutions.day20
  (:require [clojure.string :as str]
            [clojure-aoc-util.util :as aoc]
            [clojure.core.match :refer [match]]))

(defn- parse []
  (letfn [(add-sources [inp]
            (let [all-keys (into #{} (map first inp))]
              (reduce (fn [acc1 [source {:keys [targets]}]]
                        (reduce (fn [acc2 target]
                                  (update-in acc2 [target :sources] #(conj % {source :lo})))
                                acc1
                                (filter #(contains? all-keys %) targets)))
                      inp
                      inp)))]
    (->> (slurp "../inputs/day20.txt")
         str/split-lines
         (into {}
               (comp (map (partial (partial re-seq #"broadcaster|[%&]*[a-z]+")))
                     (map (fn [[s & ts]]
                            (let [s* (apply str (drop 1 s)), m* {:targets ts, :sources {}}]
                              (cond
                                (str/starts-with? s "%") {s* (merge {:type :off}         m*)}
                                (str/starts-with? s "&") {s* (merge {:type :conjunction} m*)}
                                :else                    {s  (merge {:type :broadcaster} m*)}))))))
         add-sources)))

(defn- simulate
  ([input] (simulate input nil))
  ([input look-for?]
   (letfn [(up [kw p ts]
             (update p kw (partial + (count ts))))
           (add-new [kw from rst targets]
             (concat rst (map (fn [to] [from kw to]) targets)))
           (flick [kw]
             (case kw :on :off, :off :on))
           (type->pulse [kw]
             (case kw :on :lo, :off :hi))]
     (loop [inp input, queue [["button" :lo "broadcaster"]], pulses {:lo 1, :hi 0}]
       (if (empty? queue)
         [inp pulses]
         (let [[from pulse to] (first queue), rst (rest queue)
               {:keys [type targets]} (get inp to)]
           (if (and look-for? (= from look-for?) (= pulse :hi))
             {:found look-for?}
             (case type
               nil (recur inp rst pulses) ; Is not a source itself.
               :broadcaster (recur inp (add-new :lo to rst targets) (up :lo pulses targets))
               :conjunction (let [inp2 (update-in inp [to :sources from] (constantly pulse))
                                  lh (if (every? (fn [[_ p]] (= p :hi)) (:sources (get inp2 to)))
                                       :lo
                                       :hi)]
                              (recur inp2 (add-new lh to rst targets) (up lh pulses targets)))
               ;; On or off
               (if (= pulse :hi)
                 (recur inp rst pulses)
                 (recur (update-in inp [to :type] flick)
                        (add-new (type->pulse type) to rst targets)
                        (up (type->pulse type) pulses targets)))))))))))

(defn- simulate-all [input]
  (letfn [(add [{l1 :lo, h1 :hi} {l2 :lo, h2 :hi}]
            {:lo (+ l1 l2), :hi (+ h1 h2)})]
    (iterate (fn [[inp total]]
               (let [[inp2 total2] (simulate inp)]
                 [inp2 (add total total2)]))
             [input {:lo 0, :hi 0}])))

(defn- simulate-for [input look-for]
  (loop [i 0, inp input]
    (match (simulate inp look-for)
           {:found _} (+ i 1)
           [inp2 _] (recur (+ i 1) inp2))))

(defn day20 [p]
  (let [inp (parse)]
    (case p
      :one (let [{:keys [lo hi]} (second (nth (simulate-all inp) 1000))]
             (* lo hi))
      ;; This works because xn is a conjunction, and all of its ancestors are
      ;; as well. Whoops :)
      :two (->> (get inp "xn") :sources keys
                (pmap #(simulate-for inp %))
                (reduce aoc/lcm)))))
