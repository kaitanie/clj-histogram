(defproject clj-histogram "0.1.0"
  :description "Simple histogramming library for Clojure"
  :url "http://gihub.com/kaitanie/clj-histogram"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.4.0" :exclusions [incanter/incanter-mongodb]]
                 [midje "1.4.0"]
                 [org.clojure/tools.nrepl "0.2.0-beta9"]]
  :plugins [[lein-midje "2.0.0-SNAPSHOT"]]
)
