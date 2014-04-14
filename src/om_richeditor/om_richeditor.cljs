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
                  :cursor {:focusOffset 0
                           :focusOwner nil
                           :anchorOffset 0
                           :anchorOwner nil}}))


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

(defn dom-cursor->cursor [dom-cursor focusOwner anchorOwner]
     {:focusOffset (:focusOffset dom-cursor)
      :anchorOffset (:anchorOffset dom-cursor)
      :focusOwner focusOwner
      :anchorOwner anchorOwner})

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


;; TODO: Would it be better to user graft for this?
(defn comp-cursor [data owner]
  (reify
    om/IWillMount
    (will-mount [this]
                (go (loop []
                      (let [cursor-cmd (<! (om/get-shared owner :cursor-chan))
                            cmd-type (:type cursor-cmd)
                            args (:args cursor-cmd)]
                        (condp = cmd-type
                         :inc
                          (do
                            (om/transact! data
                                          (fn [xs] (assoc xs :focusOffset (inc (:focusOffset xs))))))
                         :dec (println "dec")
                         :click
                          (let [{:keys [cursor]} args]
                            (println "Cursor notified of click" cursor)
                            (om/update! data cursor))
                         :render
                          (let [rng (-> js/document .createRange)
                                owner (:focusOwner @data)
                                focusOffset (:focusOffset @data)
                                node (om/get-node owner)
                                child (.-firstChild node)]
                            (.setStart rng child focusOffset)
                            (set-dom-cursor rng))
                          (println "Unknown cursor command")))
                      (recur))))
    om/IRenderState
    (render-state [this state]
                  (println "Cursor dom-node: " data)
                  (dom/div nil nil))))


(defn comp-terminal-node [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:keypress-chan (chan)})
    om/IWillMount
    (will-mount [_]
                (let [keypress-chan (om/get-state owner :keypress-chan)
                      cursor-chan (om/get-shared owner :cursor-chan)]
                  (go (loop []
                        (let [{:keys [keycode cursor]} (<! keypress-chan)
                              {:keys [focusOffset]} cursor
                              char (.fromCharCode js/String keycode)
                              node (om/get-node owner)]
                          (println "Terminal got a keypress!" node char cursor focusOffset)
                          (om/transact! data :text
                                        (fn [text]
                                          (str (subs text 0 focusOffset) char (subs text focusOffset))))
                          (put! cursor-chan {:type :inc}))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (let [attrs {:onClick (fn [e]
                                          (let [click-chan (om/get-shared owner :click-chan)
                                                keypress-chan (om/get-state owner :keypress-chan)]
                                            (println "Click from terminal: " @data (get-dom-cursor))
                                            (put! click-chan {:keypress-chan keypress-chan
                                                              :cursor (dom-cursor->cursor (get-dom-cursor) owner owner)})))}]
                    (json-terminal-node->om-node data attrs)))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                ;; Update the cursor location
                (let [node (om/get-node owner)
                      rng (-> js/document .createRange)
                      child (.-firstChild node)
                      cursor-chan (om/get-shared owner :cursor-chan)]
                  #_(put! cursor-chan {:type :render})
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
                 })
    om/IWillMount
    (will-mount [_]
                (let [click-chan (om/get-shared owner :click-chan)]
                  (go (loop []
                        (let [{:keys [keypress-chan cursor]} (<! click-chan)
                              cursor-chan (om/get-shared owner :cursor-chan)]
                          (println "Testing cursor-chan")
                          (put! cursor-chan {:type :click
                                             :args {:cursor cursor}})
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
                                                               (put! keypress-chan {:keycode (.. e -which)
                                                                                    :cursor (:cursor @data)})))}
                                  (om/build-all comp-node (:dom data)))
                           (om/build comp-cursor (:cursor data))))))


(let [click-chan (chan)
      cursor-chan (chan)]
  (om/root comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     :cursor-chan cursor-chan ;; This chan is to send notifications to the cursor that something has changed
                     }}))


;; (do
;;   (println "------------")
;;   (dom/render-to-str (om/build comp-keytest data4)))
