(defproject euler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-difftest "2.0.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :dependencies [[org.clojure/clojure "1.9.0-alpha5"]
                 [criterium "0.4.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [prismatic/schema "1.1.3"]])
