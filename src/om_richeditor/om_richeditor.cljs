(ns sthomp.om-richeditor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.data :as data]
            [clojure.string :as string]))


(enable-console-print!)

(def data3 [{:type "p" :children [{:type "span" :text "Child1"}
                                  {:type "span" :text "Child2"}]}])

(def data4 [{:type "a" :text "Some Terminal Node2" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}
            {:type "span" :text "Hello world "}
            {:type "p" :children [{:type "span" :text "Child1"}
                                  {:type "span" :text "Child2"}
                                  {:type "p" :children [{:type "span" :text "GrandChild1"}
                                                        {:type "span" :text "GrandChild2"}
                                                        {:type "p" :children [{:type "span" :text "GrandChild2"}
                                                                              {:type "span" :text "GrandChild3"}]}]}]}
            {:type "p" :text "Another line..."}])




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

(defn json-terminal-node->om-node
  "additional-attrs is an *optional* map of additional attributes."
  ([json-node]
     (json-terminal-node->om-node json-node {}))
  ([json-node additional-attrs]
     (let [text (:text json-node)
           attrs (merge (:attrs json-node) additional-attrs)
           js-attrs (clj->js attrs)]
       (println js-attrs)
       (println json-node)
       (case (:type json-node)
         "span" (dom/span js-attrs text)
         "em" (dom/em js-attrs text)
         "p" (dom/p js-attrs text)
         "a" (dom/a js-attrs text)
         (throw (js/Error. (str "Unknown node type: " (:type json-node))))))))

;; (dom/render-to-str (json-terminal-node->om-node (nth data4 0) {:data-test "test"}))

(defn comp-terminal-node [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (let [node (json-terminal-node->om-node data {:contentEditable true :onKeyDown #(println "blah2")})]
                    node))))

(defn comp-node [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (if (contains? data :children)
                    (do
                      (:type data)   ;; TODO: Need to use the type as the node
                      (apply dom/pre nil
                             (om/build-all comp-node (:children data))))
                    (do
                      (om/build comp-terminal-node data))))))


(defn comp-richeditor [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (apply dom/div nil
                         (om/build-all comp-node data)))))


(om/root comp-richeditor data4
  {:target (. js/document (getElementById "editor"))})

;; (do
;;   (println "------------")
;;   (dom/render-to-str (om/build comp-keytest data4)))
