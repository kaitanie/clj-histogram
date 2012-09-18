(defproject clj-histogram "0.0.1-SNAPSHOT"
  :description "Spallation plotting application"
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.4.0"]
		 [incanter "1.3.0" :exclusions [incanter/incanter-mongodb]]
                 [org.clojure/tools.nrepl "0.2.0-beta9"]]
)
