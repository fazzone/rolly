(defproject rolly "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.hypirion/clj-xchart "0.2.0"]
                 [http-kit "2.3.0"]
                 [cheshire "5.8.1"]
                 [org.clojure/data.json "0.2.6"]
                 [stylefruits/gniazdo "1.0.0"]
                 [com.cemerick/pomegranate "1.1.0"]
                 [clj-http "3.9.1"]]
  :plugins [[cider/cider-nrepl "0.21.1"]]
  :main ^:skip-aot rolly.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
