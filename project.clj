(defproject om-richeditor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.6.2"]
                 [figwheel "0.1.0-SNAPSHOT"]]
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.0.3"]
            [lein-figwheel "0.1.0-SNAPSHOT"]]
  :cljsbuild {
    :builds [
             
             ;; For interactive development
             ;; See http://rigsomelight.com/2014/05/01/interactive-programming-flappy-bird-clojurescript.html
             {:id "figwheelbuild"
              :source-paths ["src" "examples/bootstrap"]
              :compiler {
                         :output-to "resources/public/js/out/om-richeditor.js"
                         :output-dir "resources/public/js/out"
                         :optimizations :none
                         :source-map true}}
             ]})
