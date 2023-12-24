(defproject clojure-solutions "0.1.0-SNAPSHOT"
  :description "Solutions for (some) AOC 2023 problem in Clojure"
  :url "https://github.com/slotThe/advent"
  :license {:name "AGPL-3.0"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clojure-aoc-util "0.1.0-SNAPSHOT"]
                 [net.mikera/core.matrix "0.63.0"]
                 [net.mikera/vectorz-clj "0.48.0"]]
  :main ^:skip-aot clojure-solutions.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
