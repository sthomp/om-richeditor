(ns sthomp.om-richeditor
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.data :as data]))





#_(defmacro test22 [x]
  `(eval (symbol "clojure.core" ~x))
 ) 
 
#_(defmacro test-om [x]
  @(resolve (symbol "clojure.core" x))
  )
