(defproject om-richeditor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.5.0"]]
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :cljsbuild {
    :builds [{:id "main"
              :source-paths ["src"]
              :compiler {
                :output-to "om-richeditor.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}

             {:id "main"
              :source-paths ["src" "examples/bootstrap/src"]
              :compiler {
                :output-to "examples/bootstrap/om-richeditor.js"
                :output-dir "examples/bootstrap/out"
                :optimizations :none
                :source-map "examples/bootstrap/app.js.map"}}]})
