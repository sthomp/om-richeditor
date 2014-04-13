(ns sthomp.om-richeditor.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! close!]]
            [clojure.data :as data]
            [clojure.string :as string]))


(enable-console-print!)

(def data4 (atom {:dom [{:type "a" :text "Some Terminal Node2" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}
                        {:type "span" :text "Hello world"}
                        {:type "p" :children [{:type "span" :text "Child1"}
                                              {:type "span" :text "Child2"}
                                              {:type "p" :children [{:type "span" :text "GrandChild1"}
                                                                    {:type "span" :text "GrandChild2"}
                                                                    {:type "p" :children [{:type "span" :text "GrandChild2"}
                                                                                          {:type "span" :text "GrandChild3"}]}]}]}
                        {:type "p" :text "Another line..."}]
                  :cursor {:focusOffset 0}}))


(defn set-dom-cursor [range]
  (let [selection (-> js/window .getSelection)]
    (.removeAllRanges selection)
    (.addRange selection range)
    nil))

(defn get-dom-cursor []
  (let [selection (-> js/window .getSelection)]
    {:focusNode (.-focusNode selection)
     :anchorNode (.-anchorNode selection)
     :focusOffset (.-focusOffset selection)
     :anchorOffset (.-anchorOffset selection)}))


(defn json-terminal-node->om-node
  "additional-attrs is an *optional* map of additional attributes."
  ([json-node]
     (json-terminal-node->om-node json-node {}))
  ([json-node additional-attrs]
     (let [text (:text json-node)
           attrs (merge (:attrs json-node) additional-attrs)
           js-attrs (clj->js attrs)]
       (case (:type json-node)
         "span" (dom/span js-attrs text)
         "em" (dom/em js-attrs text)
         "p" (dom/p js-attrs text)
         "a" (dom/a js-attrs text)
         (throw (js/Error. (str "Unknown node type: " (:type json-node))))))))

(defn comp-cursor [data owner {:keys [cursor-chan] :as opts}]
  (reify
    om/IWillMount
    (will-mount [this]
                (go (loop []
                      (let [cursor-cmd (<! cursor-chan)
                            cmd-type (:type cursor-cmd)
                            args (:args cursor-cmd)]
                        (println "cursor-cmd" cursor-cmd)
                        (condp = cmd-type
                         :add-chr (println "add-char")
                         :backspace (println "backspace")
                         :del (println "del")
                         :click (println "click")
                         (println "Unknown cursor command")))
                      (recur))))
    om/IRenderState
    (render-state [this state]
                  (dom/div nil "Cursor"))))


(defn comp-terminal-node [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:keypress-chan (chan)})
    om/IWillMount
    (will-mount [_]
                (let [keypress-chan (om/get-state owner :keypress-chan)]
                  (go (loop []
                        (let [key-code (<! keypress-chan)
                              char (.fromCharCode js/String key-code)
                              node (om/get-node owner)]
                          (println "I got a keypress!" node char)
                          (om/transact! data :text
                                        (fn [xs] (str xs char))))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (let [node (json-terminal-node->om-node data {:onClick (fn [e]
                                                                           (let [click-chan (om/get-shared owner :click-chan)
                                                                                 keypress-chan (om/get-state owner :keypress-chan)]
                                                                             (println "Current cursor: " (get-dom-cursor))
                                                                             (put! click-chan keypress-chan)))})]
                    node))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                ;; Update the cursor location
                (let [node (om/get-node owner)
                      rng (-> js/document .createRange)
                      child (.-firstChild node)]
                  (.selectNode rng child)
                  (.collapse rng false)
                  (set-dom-cursor rng)))))

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
    om/IInitState
    (init-state [_]
                {:keypress-chan nil   ;; This chan is set from child terminal nodes when they become focused so we can notify them of keypress
                 :cursor-chan (chan)  ;; This chan is to send notifications to the cursor that something has changed
                 })
    om/IWillMount
    (will-mount [_]
                (let [click-chan (om/get-shared owner :click-chan)]
                  (go (loop []
                        (let [keypress-chan (<! click-chan)
                              cursor-chan (om/get-state owner :cursor-chan)]
                          (put! cursor-chan {:type :click
                                             :args nil})
                          (om/set-state! owner :keypress-chan keypress-chan))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (dom/div nil
                           (apply dom/div #js {:contentEditable true
                                               :onKeyDown (fn [e]
                                                            #_(println "keydown"))
                                               :onKeyPress (fn [e]
                                                             #_(println "keypress")
                                                             (.preventDefault e)
                                                             (when-let [keypress-chan (:keypress-chan (om/get-state owner))]
                                                               (put! keypress-chan (.. e -which))))}
                                  (om/build-all comp-node (:dom data)))
                           (om/build comp-cursor (:cursor data) {:opts {:cursor-chan (:cursor-chan state)}})))))


(let [click-chan (chan)]
  (om/root comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     }}))


;; (do
;;   (println "------------")
;;   (dom/render-to-str (om/build comp-keytest data4)))
