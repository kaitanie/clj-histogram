(defproject clj-histogram "0.3.1"
  :description "Simple histogramming library for Clojure"
  :url "http://github.com/kaitanie/clj-histogram"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.4.1" :exclusions [incanter/incanter-mongodb]]
                 [midje "1.4.0"]
                 [org.clojure/tools.nrepl "0.2.0-RC1"]]
  :plugins [[lein-midje "2.0.1"]])
