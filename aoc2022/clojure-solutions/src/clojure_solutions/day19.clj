(ns clojure-solutions.day19
  (:require [clojure.string :as str])
  (:use [clojure-solutions.util] :reload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types!

(defrecord OreBot      [^long ore])
(defrecord ClayBot     [^long ore])
(defrecord ObsidianBot [^long ore, ^long clay])
(defrecord GeodeBot    [^long ore, ^long obsidian])

(defrecord Blueprint [^OreBot      ore-bot
                      ^ClayBot     clay-bot
                      ^ObsidianBot obsidian-bot
                      ^GeodeBot    geode-bot])

(defrecord State [^long ore-bots      ^long ore
                  ^long clay-bots     ^long clay
                  ^long obsidian-bots ^long obsidian
                  ^long geode-bots    ^long geode])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defn- parse []
  (->> (slurp "../inputs/day19.txt")
       str/split-lines
       (map (comp (partial map read-string) (partial re-seq #"\d+")))
       (map (fn [[_ oro cro obro obrc gro grob]]
              (->Blueprint (->OreBot oro)
                           (->ClayBot cro)
                           (->ObsidianBot obro obrc)
                           (->GeodeBot gro grob))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mining step

(defn- kw->kw
  "Create a new keyword from 'kw' with 'suf' appended to it."
  [^clojure.lang.Keyword kw ^String suf]
  (keyword (str/join [(name kw) suf])))

(defn- mine
  "Update the 'state' with all the new rocks we mined."
  [^State state]
  (reduce (fn [s k] (update s k (partial + (get s (kw->kw k "-bots")))))
          state
          [:ore :clay :obsidian :geode]))

(defn- step
  "A single step in the process.

  In addition to the \"can we afford this?\" conditions, there is one
  more set.  Since we can only buy one thing per round anyways, there is
  no sense having more X bots than we need to build the next highest
  bot.  For example, we don't need more obsidian bots than is needed to
  build a single geode bot; that would just be a waste.

  Further, always test ore bots, but prefer the obvious order for the
  other bots.  Why does this work?  I don't know; found it out through
  trial and error.  It's probably a fault with my input, but it keeps me
  from having to think of creative ways of pruning :)"
  [^Blueprint {:keys [ore-bot clay-bot obsidian-bot geode-bot] :as blueprint}
   ^State {:keys [ore-bots ore clay-bots clay obsidian-bots obsidian] :as s}]
  (letfn [(update-state [^clojure.lang.Keyword kw]
            "Update the state for the given keyword."
            (let [kws (get blueprint (kw->kw kw "-bot"))]
              (reduce (fn [st k]
                        (update st k #(- % (get kws k))))
                      (update (mine s) (kw->kw kw "-bots") inc)
                      (keys kws))))]
    (filter some?
            [(when (and (>= ore (:ore ore-bot))
                        (< ore-bots
                           (apply max (map :ore [ore-bot clay-bot obsidian-bot geode-bot]))))
               (update-state :ore))
             (cond
               (and (>= ore (:ore geode-bot))
                    (>= obsidian (:obsidian geode-bot)))
               (update-state :geode)

               (and (>= ore (:ore obsidian-bot))
                    (>= clay (:clay obsidian-bot))
                    (< obsidian-bots (:obsidian geode-bot)))
               (update-state :obsidian)

               (and (>= ore (:ore clay-bot))
                    (< clay-bots (:clay obsidian-bot)))
               (update-state :clay))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pruning

(defn- better-than
  "Is 's1' \"better\" than 's2'?
  The implementation is *really* sad, but also by far the fastest one
  I've tried."
  [^State s1 ^State s2]
  (and (>= (:ore s1) (:ore s2))
       (>= (:ore-bots s1) (:ore-bots s2))
       (>= (:clay s1) (:clay s2))
       (>= (:clay-bots s1) (:clay-bots s2))
       (>= (:obsidian s1) (:obsidian s2))
       (>= (:obsidian-bots s1) (:obsidian-bots s2))
       (>= (:geode s1) (:geode s2))
       (>= (:geode-bots s1) (:geode-bots s2))))

(defn- prune-better [ss]
  (reduce (fn [acc el]
            (if (some #(better-than % el) acc) ; Anything better than 'el'?
              acc
              ;; If not, only keep the items that are at least as good.
              (conj (filter #(not (better-than el %)) acc)
                    el)))
          []
          ss))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solver

(defn- solve ^long [^long t-start ^Blueprint blueprint]
  (loop [t      t-start
         states [(->State 1 0 0 0 0 0 0 0)]
         seen   #{}]
    (if (= t 0)
      (:geode (apply max-key :geode states))
      (let [ss (prune-better
                (into #{}
                      (comp cat (filter #(not (contains? seen %))))
                      (conj (map (partial step blueprint) states)
                            (map mine states))))]
        (recur (dec t) ss (apply conj seen ss))))))

(defn day19 ^long [^clojure.lang.Keyword p]
  (let [inp (parse)]
    (case p
      :one (->> inp
                (map (partial solve 24))
                (map * (drop 1 (range)))
                sum)
      :two (->> (take 3 inp)
               (map (partial solve 32))
               (reduce *)))))
