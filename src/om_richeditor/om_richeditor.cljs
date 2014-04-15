(ns sthomp.om-richeditor
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! close!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [goog.dom :as gdom]))


(enable-console-print!)

(def data4 (atom {:dom [{:type "a" :text "Some Terminal Node2" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}
                        {:type "span" :text "Hello world"}
                        {:type "p" :children [{:type "span" :text "Child1"}
                                              {:type "span" :text "Child2"}
                                              {:type "p" :children [{:type "span" :text "GrandChild1"}
                                                                    {:type "span" :text "GrandChild2"}
                                                                    {:type "p" :children [{:type "span" :text "GrandChild2"}
                                                                                          {:type "span" :text "GrandChild3"}]}]}]}
                        {:type "p" :text "Another line..."}]}))


(defn set-dom-caret [range]
  (let [selection (-> js/window .getSelection)]
    (.removeAllRanges selection)
    (.addRange selection range)
    nil))

(defn get-dom-caret []
  (let [selection (-> js/window .getSelection)]
    {:focusNode (.-focusNode selection)
     :anchorNode (.-anchorNode selection)
     :focusOffset (.-focusOffset selection)
     :anchorOffset (.-anchorOffset selection)}))

(defn dom-caret->caret [dom-caret focusOwner anchorOwner]
     {:focusOffset (:focusOffset dom-caret)
      :anchorOffset (:anchorOffset dom-caret)
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



(defn keycode->char [keycode]
  (condp = keycode
    32 "\u00a0"
    (.fromCharCode js/String keycode)))


(defn caret-action [caret {cmd-type :type
                           args :args}]
  (println "CARET ACTION" caret cmd-type)
  (condp = cmd-type
    :inc (do
           (println "Caret notified of inc")
           (swap! caret (fn [xs]
                             (println "swap! " xs)
                             (assoc xs
                               :focusOffset (inc (:focusOffset xs))
                               :anchorOffset (inc (:anchorOffset xs)))))
           (println caret))
    :dec (println "dec")
    :click (let [{new-caret :caret} args]
             (println "new-caret" new-caret)
             (reset! caret new-caret))
    :render (let [rng (-> js/document .createRange)
                  owner (-> @caret :focusOwner)
                  focusOffset (-> @caret :focusOffset)
                  node (om/get-node owner)
                  child (.-firstChild node)
                  ]
              (.setStart rng child focusOffset)
              (set-dom-caret rng))
    (.log js/console "Unknown caret command")))


(defn comp-terminal-node [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:keypress-chan (chan)})
    om/IWillMount
    (will-mount [_]
                (let [keypress-chan (om/get-state owner :keypress-chan)]
                  (go (loop []
                        (let [{:keys [keycode]} (<! keypress-chan)
                              caret (om/get-shared owner :caret)
                              {:keys [focusOffset]} @caret
                              char (keycode->char keycode)
                              node (om/get-node owner)]
                          (println "Terminal got a keypress!" node char caret focusOffset)
                          (om/transact! data :text
                                        (fn [text]
                                          (str (subs text 0 focusOffset) char (subs text focusOffset))))
                          (caret-action caret {:type :inc}))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (let [attrs {:onClick (fn [e]
                                          (let [click-chan (om/get-shared owner :click-chan)
                                                keypress-chan (om/get-state owner :keypress-chan)]
                                            #_(println "Click from terminal: " @data (get-dom-caret))
                                            (.log js/console (str "Click from terminal: " @data (get-dom-caret)))
                                            (caret-action (om/get-shared owner :caret) {:type :click
                                                                                        :args {:caret (dom-caret->caret (get-dom-caret) owner owner)}})
                                            (put! click-chan {:keypress-chan keypress-chan})))}]
                    (json-terminal-node->om-node data attrs)))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                ;; Update the caret location
                (let [caret (om/get-shared owner :caret)]
                  (caret-action caret {:type :render})))))

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

;; Keycodes
(def BACKSPACE 8)
(def DELETE 46)
(def LEFTARROW 37)
(def RIGHTARROW 39)
(def UPARROW 38)
(def DOWNARROW 40)



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
                        (let [{:keys [keypress-chan]} (<! click-chan)]
                          (om/set-state! owner :keypress-chan keypress-chan))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (dom/div nil
                           (apply dom/div #js {:contentEditable true
                                               :spellCheck false
                                               :onKeyDown (fn [e]
                                                            (let [keycode (.-which e)]
                                                              (println "keydown " keycode)
                                                              (cond
                                                               (= BACKSPACE keycode) (do
                                                                                       (println "Backspace")
                                                                                       (.preventDefault e))
                                                               (= DELETE keycode) (do
                                                                                    (println "Del")
                                                                                    (.preventDefault e))
                                                               (#{UPARROW DOWNARRAY RIGHTARROW LEFTARROW} keycode) (do
                                                                                                                     (println "update caret"))
                                                               :else nil)))
                                               :onKeyPress (fn [e]
                                                             #_(println "keypress")
                                                             (.preventDefault e)
                                                             (when-let [keypress-chan (:keypress-chan (om/get-state owner))]
                                                               (put! keypress-chan {:keycode (.. e -which)})))}
                                  (om/build-all comp-node (:dom data)))))))


(let [click-chan (chan)]
  (om/root comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     :caret (atom {:focusOffset 0
                                   :focusOwner nil
                                   :anchorOffset 0
                                   :anchorOwner nil})
                     }}))


;; (do
;;   (println "------------")
;;   (dom/render-to-str (om/build comp-keytest data4)))
