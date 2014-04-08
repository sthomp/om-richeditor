(ns sthomp.om-richeditor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.data :as data]
            [clojure.string :as string]))


(enable-console-print!)


(def app-state (atom {:dom [(dom/p #js {:data-row 0} "Row1")
                            (dom/p #js {:data-row 1} "Row2")
                            (dom/p #js {:data-row 2} "Row3")]
                      :cursor 0}))



(JSON/parse "{\"list\": \"[1,2,3,4,5]\", \"blah\": \"vtha\", \"o\": {\"answer\": \"42\"}}")

(defn set-cursor [range]
  (let [selection (-> js/window .getSelection)]
    (.removeAllRanges selection)
    (.addRange selection range)
    nil))


(defn inner-widget [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (println "blah")
                  data)))
(count [1 2 3])

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
                     rng (-> js/document .createRange)]
                 (println (.getAttribute node "data-row"))
                 (.setStart rng node 1)
                 (.collapse rng)
                 (set-cursor rng)))))

(om/root widget app-state
  {:target (. js/document (getElementById "editor"))})
