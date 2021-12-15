(defproject clojure-solutions "0.1.0-SNAPSHOT"
  :description "Clojure solutions for Advent of Code 2021"
  :url "https://gitlab.com/slotThe/aoc-2021"
  :license {:name "AGPL-3.0"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :main ^:skip-aot clojure-solutions.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
