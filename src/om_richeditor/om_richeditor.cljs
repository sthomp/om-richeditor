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
                               {:type "a" :text "this is a link" :href "http://www.google.com"}]}
         {:type "p" :text "Next Paragraph"}]})

(def data2 {:type "a" :text "Some Terminal Node" :href "http://www.google.com"})
(def data3 [{:type "a" :text "Some Terminal Node" :href "http://www.google.com"}
            {:type "span" :text "Hello world "}])
(def data4 [{:type "a" :text "Some Terminal Node2" :href "http://www.google.com"}
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

(defn construct-node-with-children [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (println "node-with-children")
                  (om/build-all
                     json-node->om-node
                     (:dom data)
                     {:init-state state}))))

(defn json-terminal-node->om-node [json-node]
  (let [text (:text json-node)]
    (case (:type json-node)
      "span" (dom/span nil text)
      "em" (dom/em nil text)
      "p" (dom/p nil text)
      "a" (dom/a #js {:href "http://www.yahoo.com" :rel "nofollow" :target "_blank"} text)
      (throw (js/Error. (str "Unknown node type: " (:type json-node)))))))


(defn construct-terminal-node [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (println "terminal-node")
                  (let [om-node (json-terminal-node->om-node data)]
                    (println om-node))
                  (dom/span nil ()))))

(defn json-node->om-node [json-node]
  (if (contains? json-node :children)
    (construct-node-with-children (:children json-node owner))
    (construct-terminal-node)))


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

(defn comp-node-with-children [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (println "Building with data: " data)
                  (apply dom/pre nil
                         (map (fn [child]
                           (println "Processing child: " child)
                           (if (contains? child :children)
                             (do
                               (println "contains children")
                               (println (:children child))
                               (let [build-all-node (apply dom/div nil
                                      om/build-all comp-node-with-children2 (:children child))]
                                 (println (dom/render-to-str build-all-node)))
                               (om/build comp-node-with-children (:children child)))
                             (do
                               (println "no children")
                               (om/build comp-terminal-node child)))) data)))))


(do
  (println "------------")
  (dom/render-to-str (om/build comp-node-with-children data4)))

(om/root comp-richeditor data4
  {:target (. js/document (getElementById "editor"))})


(defn inner-widget [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (println "blah")
                  data)))

(defn widget [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:contentEditable true
                    :onKeyDown (fn [e]
                                 (println (-> js/window
                                              .getSelection
                                              .-focusOffset))
                                 (.preventDefault e)
                                 (println (.-keyCode e))
                                 (case (.-keyCode e)
                                   13 (om/transact! data :dom
                                                    (fn [xs]
                                                      (let [len (-> xs count inc)
                                                             new-node (dom/p #js {:data-row len} (str "Row" len))]
                                                         (conj xs new-node))))
                                   nil))}
               (apply dom/ul nil
                      (om/build-all inner-widget (:dom data)
                                    {:init-state state}))))
    om/IDidMount
    (did-mount [this]
               (let [node (om/get-node (nth (:dom data) 0))
                     rng (-> js/document .createRange)
                     child (.-firstChild node)]
                 (println (.getAttribute node "data-row"))
                 (.setStart rng child 1)
                 (.collapse rng)
                 (set-cursor rng)))))



