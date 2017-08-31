(defproject calculator-shortest-path "0.1.0-SNAPSHOT"
  :description "Calculates the path between two values given a predefined set of operations using two-way BFS"
  :url "https://github.com/nwrahnem/calculator-shortest-path"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot calculator-shortest-path.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
