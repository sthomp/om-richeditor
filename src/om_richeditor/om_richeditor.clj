(ns sthomp.om-richeditor
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! close!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [goog.dom :as gdom]))


(def my-code '(println "techbehindtech"))
my-code
(eval my-code)



(defmacro def-logged-fn [fn-name args & body]
        `(defn ~fn-name ~args
           (println "Calling ...")
           ~@body))

(def-logged-fn say[name]
        (println (str "hello " name)))

(say "arg")

(macroexpand-1 '(def-logged-fn say[name]
        (println (str "hello " name))))

(defmacro create-om [x]
  `(om.dom (symbol ~x)))
(symbol "om.dom" "p")
(resolve (symbol "p"))
(macroexpand-1 `(create-om "p"))
(create-om "test2")

(defn test [tag]
  (str "React.DOM." (name tag)))
(test "p")

(str "React.DOM." "p")
`(binding [om.dom p])
(symbol "foo")

(defmacro allow-reads [& body]
  `(binding [om.core/*read-enabled* true]
    ~@body))
(clojure.core/binding [om.core/*read-enabled* true] (clojure.core/println "test"))

(macroexpand-1 `(allow-reads (println "test")))
(map #((resolve (symbol %1)) %2) ["println" "print" "prn"] ["asdf" "asdf" "asdf"])
