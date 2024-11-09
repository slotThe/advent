(defproject clojure-solutions "0.1.0-SNAPSHOT"
  :description "Clojure soluctions for AoC 2024"
  :url "https://github.com/slotThe/advent"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clojure-aoc-util "0.1.0-SNAPSHOT"]]
  :main ^:skip-aot clojure-solutions.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
