(defproject om-richeditor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.6.0"]]
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.0.2"]]
  :cljsbuild {
    :builds [{:id "main"
              :source-paths ["src"]
              :compiler {
                :output-to "om-richeditor.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}

             {:id "example_bootstrap"
              :source-paths ["src" "examples/bootstrap/src"]
              :compiler {
                :output-to "examples/bootstrap/om-richeditor.js"
                :output-dir "examples/bootstrap/out"
                :optimizations :none
                :source-map true}}]})
