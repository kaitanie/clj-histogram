(defproject clj-histogram "0.0.2-SNAPSHOT"
  :description "Simple histogramming library for Clojure"
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.3.0" :exclusions [incanter/incanter-mongodb]]
                 [midje "1.4.0"]
                 [org.clojure/tools.nrepl "0.2.0-beta9"]]
  :plugins [[lein-midje "2.0.0-SNAPSHOT"]]
)
