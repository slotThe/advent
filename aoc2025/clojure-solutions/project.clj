(defproject clojure-solutions "0.1.0-SNAPSHOT"
  :description "Clojure solutions for AoC2025"
  :url "https://github.com/slotThe/advent"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.12.3"]]
  :main ^:skip-aot clojure-solutions.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
