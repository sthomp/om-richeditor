(ns sthomp.om-richeditor
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defmacro get-om-node [tag]
  `(om.dom/p nil "hello"))



(macroexpand (get-om-node "p"))
