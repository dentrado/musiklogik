(defproject musiklogik "0.1.0-SNAPSHOT"
  :description "Music logic experiments"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.3" :exclusions [org.clojure/clojure]]
                 [overtone "0.8.1" :exclusions [org.clojure/clojure]]
                 [leipzig "0.4.0"]]
  :jvm-opts ["-Xmx2g"])
