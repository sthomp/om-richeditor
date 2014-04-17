(ns sthomp.om-richeditor
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! close!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [goog.dom :as gdom]))


(enable-console-print!)

(def data4 (atom {:dom [{:tag "a" :text "Some Terminal Node2" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}
                        {:tag "span" :text "Hello world"}
                        {:tag "p" :children [{:tag "span" :text "Child1"}
                                              {:tag "span" :text "Child2"}
                                              {:tag "p" :children [{:tag "span" :text "GrandChild1"}
                                                                    {:tag "span" :text "GrandChild2"}
                                                                    {:tag "p" :children [{:tag "span" :text "GrandChild2"}
                                                                                          {:tag "span" :text "GrandChild3"}]}]}]}
                        {:tag "p" :text "Another line..."}]
                  :caret {:path [:dom 1 :children 0]
                          :focusOffset 0}}))


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
       (case (:tag json-node)
         "span" (dom/span js-attrs text)
         "em" (dom/em js-attrs text)
         "p" (dom/p js-attrs text)
         "a" (dom/a js-attrs text)
         (throw (js/Error. (str "Unknown node tag: " (:tag json-node))))))))



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
                          #_(println "Terminal got a keypress!" node char caret focusOffset)
                          #_(om/transact! data :text
                                        (fn [text]
                                          (str (subs text 0 focusOffset) char (subs text focusOffset))))
                          #_(caret-action caret {:type :inc}))
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
                                            (put! click-chan {:keypress-chan keypress-chan
                                                              :path (om.core/path data)})))}]
                    (json-terminal-node->om-node data attrs)))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                ;; Update the caret location
                (println "RE-RENDER: " owner (om/path data))
                (let [caret (om/get-shared owner :caret)]
                  (caret-action caret {:type :render})))))

(defn comp-node [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (if (contains? data :children)
                    (do
                      (:tag data)   ;; TODO: Need to use the tag as the node
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

(drop-while #(not (number? %)) [:a :b 1 2 :keyword 3])


(defn path->dom-node [root-owner path]
  (let [root-dom (-> root-owner
                     om/get-node
                     .-firstChild)
        traverse-down (fn traverse-down [dom-node path]
                        (let [path-head (drop-while #(not (number? %)) path)]
                          (if-let [child-idx (first path-head)]
                            (let [child-at-path (-> dom-node .-children (aget child-idx))]
                              (traverse-down child-at-path (drop 1 path-head)))
                            dom-node)))]
    (traverse-down root-dom path)))


(defn handle-keypress [e root-data path caret-focusoffset]
  (.log js/console e)
  (condp = (.. e -type)
    "keydown" (condp = (.. e -which)
                ;; Block certain keystrokes from propogating to keypress
                ;; but let regular keystrokes pass through (like pressing a character)
                BACKSPACE (do
                            (.. e preventDefault)
                            (println "backspace"))
                DELETE (do
                         (.. e preventDefault)
                         (println "delete"))
                UPARROW (do
                          (.. e preventDefault)
                          (println "up")) 
                DOWNARROW (do
                            (.. e preventDefault)
                            (println "down"))
                LEFTARROW (do
                            (.. e preventDefault)
                            (println "left"))
                RIGHTARROW (do
                             (.. e preventDefault)
                             (println "right"))
                nil)
    "keypress" (let [keycode (.. e -which)
                     char (keycode->char keycode)]
                 (.. e preventDefault)
                 (.log js/console char)
                 (println "path " (conj path :text))
                 (om/transact! root-data (conj path :text)
                               (fn [text]
                                 (println "got text" text)
                                 (str (subs text 0 caret-focusoffset) char (subs text caret-focusoffset))))
                 (println "keypress")) 
    nil))


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
                        (let [{:keys [keypress-chan path]} (<! click-chan)]
                          (println "CLICK" path)
                          (.log js/console (path->dom-node owner path))
                          #_(om/update! data [:dom 2 :children 1] (get-in @data [:dom 2 :children 1]))
                          #_(om/transact! data [:dom 2 :children 1 :text]
                                        (fn [x]
                                          (println x)
                                          "blah"))
                          #_(om/set-state! owner :keypress-chan keypress-chan))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (let [path [:dom 1]
                        caret-focusoffset 1]
                    (om/transact! data [:caret :focusOffset] 
                                  (fn [x] 1))
                    (dom/div nil
                           (apply dom/div #js {:contentEditable true
                                               :spellCheck false
                                               :onKeyDown (fn [e]
                                                            (handle-keypress e data path caret-focusoffset))
                                               :onKeyPress (fn [e]
                                                             (handle-keypress e data path caret-focusoffset)
                                                             (when-let [keypress-chan (:keypress-chan (om/get-state owner))]
                                                               (put! keypress-chan {:keycode (.. e -which)})))}
                                  (om/build-all comp-node (:dom data))))))))


(let [click-chan (chan)]
  (om/root comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     :caret (atom {:focusOffset 0
                                   :focusOwner nil
                                   :anchorOffset 0
                                   :anchorOwner nil})}
            :tx-listen (fn [tx-data root-cursor]
                         nil )}))






(defn comp-caret [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (println data)
                  (dom/table #js {:className "caret-table"}
                             (dom/tbody nil 
                                        (dom/tr nil
                                                (dom/td nil ":path")
                                                (dom/td nil (str "[" (string/join " " (:path data)) "]")))
                                        (dom/tr nil 
                                                (dom/td nil ":focusOffset")
                                                (dom/td nil (:focusOffset data))))))))

(om/root comp-caret data4
         {:target (. js/document (getElementById "caret"))
          :path [:caret]})
