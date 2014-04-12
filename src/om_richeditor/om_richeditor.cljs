(ns sthomp.om-richeditor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.data :as data]
            [clojure.string :as string]))


(enable-console-print!)

(def data
  {:dom [{:type "p" :children [{:type "span" :text "Hello world "}
                               {:type "em" :text "this is emphasized"}
                               {:type "span" :text " this is regular "}
                               {:type "a" :text "this is a link" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}]}
         {:type "p" :text "Next Paragraph"}]})

(def data4 [{:type "a" :text "Some Terminal Node2" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}
            {:type "span" :text "Hello world "}
            {:type "p" :children [{:type "span" :text "Child1"}
                                  {:type "span" :text "Child2"}]}])




(def app-state (atom {:dom [(dom/p #js {:data-row 0} "Row1")
                            (dom/p #js {:data-row 1} "Row2")
                            (dom/p #js {:data-row 2} "Row3")]
                      :cursor {:focusOffset 0
                               :anchorOffset 0}}))


(defn set-cursor [range]
  (let [selection (-> js/window .getSelection)]
    (.removeAllRanges selection)
    (.addRange selection range)
    nil))

(defn json-terminal-node->om-node [json-node]
  "attrs is an *optional* map of attributes. If none is specified then nil will be used."
  ;; If attrs is specified then we need to do: #js attrs. otherwise, nil.
  (let [text (:text json-node)
        js-attrs (clj->js (:attrs json-node))]
    (case (:type json-node)
      "span" (dom/span js-attrs text)
      "em" (dom/em js-attrs text)
      "p" (dom/p js-attrs text)
      "a" (dom/a js-attrs text)
      (throw (js/Error. (str "Unknown node type: " (:type json-node)))))))

(dom/render-to-str (json-terminal-node->om-node (nth data4 0)))

(defn comp-terminal-node [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (json-terminal-node->om-node data))))

(defn comp-node [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (if (contains? data :children)
                    (do
                      (println "contains children")
                      (:type data)
                      (apply dom/pre nil
                             (om/build-all comp-node (:children data))))
                    (do
                      (println "no children")
                      (om/build comp-terminal-node data))))))


(defn comp-richeditor [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (apply dom/div #js {:contentEditable true}
                         (om/build-all comp-node data)))))

(do
  (println "------------")
  (dom/render-to-str (om/build comp-richeditor data4)))

(om/root comp-richeditor data4
  {:target (. js/document (getElementById "editor"))})
