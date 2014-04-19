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
                                                                  {:tag "p" :text "GrandChild3"}
                                                                  {:tag "p" :children [{:tag "span" :text "GrandChild2"}
                                                                                       {:tag "span" :text "GrandChild3"}]}]}]}
                        {:tag "p" :text "Another line..."}]
                  :caret {:focus-path []
                          :focus-offset 0
                          :anchor-offset 0
                          :anchor-path []}}))


(defn set-dom-caret
  ([range]
   (let [selection (-> js/window .getSelection)]
     (.removeAllRanges selection)
     (.addRange selection range)
     nil))
  ([dom-node focus-offset]
   (let [new-range (.createRange js/document)]
     (.setStart new-range (.-firstChild dom-node) focus-offset)
     (set-dom-caret new-range))))

(defn get-dom-caret []
  (let [selection (-> js/window .getSelection)]
    {:focusNode (.-focusNode selection)
     :anchorNode (.-anchorNode selection)
     :focus-offset (.-focusOffset selection)
     :anchor-offset (.-anchorOffset selection)}))




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





(defn comp-terminal-node [data owner]
  (reify
    om/IInitState
    (init-state [_]
                )
    om/IWillMount
    (will-mount [_]
                )
    om/IRenderState
    (render-state [this state]
                  (let [attrs {:onClick (fn [e]
                                          (let [click-chan (om/get-shared owner :click-chan)]
                                            (.log js/console (str "Click from terminal: "))
                                            (put! click-chan {:path (om.core/path data)
                                                              :current-target (.. e -currentTarget)})))}]
                    (json-terminal-node->om-node data attrs)))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                ;; Update the caret location
                (println "RE-RENDER: " owner (om/path data)))))

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

(defn up [current-path current-focus-offset]
  (drop-last 2 current-path)
  [new-path new-focus-offset])
(def case1-path [:dom 2 :children 2 :children 0])
(def case1-offset 3)    ;; Expect: [:dom 2 :children 0] and 3
(case case2-offset 10)  ;; Expect: [:dom 2 :children 1] and 4
(def y (drop-last 2 x))
(last y)


(defn probe-for-caret []
  "Forces a click on the element where the caret is"
  (let [elem (.. js/window getSelection -focusNode)]
    (.. elem -parentNode click)))

(defn caret-action [action root-data]
  "TODO: Change this so that it automatically increments/decrements by the difference in length of the text string"
  (condp = action
    :caret-inc (om/transact! root-data [:caret]
                             (fn [caret]
                               (let [{:keys [focus-offset anchor-offset]} caret]
                                 (assoc caret
                                   :focus-offset (inc focus-offset)
                                   :anchor-offset (inc anchor-offset)))))
    :caret-dec (om/transact! root-data [:caret]
                             (fn [caret]
                               (let [{:keys [focus-offset anchor-offset]} caret]
                                 (assoc caret
                                   :focus-offset (dec focus-offset)
                                   :anchor-offset (dec anchor-offset)))))
    (throw (js/Error. (str "Unknown caret-action: " action)))))


(defn handle-keypress [e root-data]
  (condp = (.. e -type)
    "keydown" (condp = (.. e -which)
                BACKSPACE (do
                            (.. e preventDefault)
                            (println "backspace"))
                DELETE (do
                         (.. e preventDefault)
                         (println "delete"))
                nil)
    "keyup" (condp = (.. e -which)
                ;; Block certain keystrokes from propogating to keypress
                ;; but let regular keystrokes pass through (like pressing a character)
                UPARROW (do
                          (probe-for-caret)
                          (println "up"))
                DOWNARROW (do
                            (probe-for-caret)
                            (println "down"))
                LEFTARROW (do
                            (probe-for-caret)
                            (println "left"))
                RIGHTARROW (do
                             (probe-for-caret)
                             (println "right"))
                nil)
    "keypress" (let [keycode (.. e -which)
                     char (keycode->char keycode)
                     caret (-> @root-data :caret)
                     focus-path (:focus-path caret)
                     focus-offset (:focus-offset caret)
                     anchor-offset (:anchor-offset caret)]
                 (.. e preventDefault)
                 (om/transact! root-data (conj focus-path :text)
                               (fn [text]
                                 (str (subs text 0 focus-offset) char (subs text focus-offset))))
                 (caret-action :caret-inc root-data)
                 (println "keypress"))
    nil))


(defn comp-richeditor [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:dom-caret nil})
    om/IWillMount
    (will-mount [_]
                (let [click-chan (om/get-shared owner :click-chan)]
                  ;; Listen for clicks on terminal nodes so we can reset the caret
                  (go (loop []
                        (let [{:keys [path current-target]} (<! click-chan)
                              dom-caret (get-dom-caret)
                              dom-node-caret (.. (:focusNode dom-caret) -parentNode)
                              new-caret (if (identical? dom-node-caret current-target)
                                          {:focus-offset (:focus-offset dom-caret)
                                           :anchor-offset (:anchor-offset dom-caret)
                                           :focus-path path
                                           :anchor-path path}
                                          {:focus-offset 0
                                           :anchor-offset 0
                                           :focus-path path
                                           :anchor-path path})]
                          (println "CLICK" (.. (:focusNode dom-caret) -parentNode) " TO " current-target)
                          (om/update! data :caret new-caret))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (dom/div nil
                           (apply dom/div #js {:contentEditable true
                                               :spellCheck false
                                               :onKeyUp (fn [e]
                                                            (handle-keypress e data))
                                               :onKeyPress (fn [e]
                                                             (handle-keypress e data))}
                                  (om/build-all comp-node (:dom data)))))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                ;; Update the caret location
                (println "Richeditor Rerender")
                (let [focus-path (-> data :caret :focus-path)
                      dom-node (path->dom-node owner focus-path)
                      focus-offset (-> data :caret :focus-offset)]
                  (set-dom-caret dom-node focus-offset)))))


(let [click-chan (chan)]
  (om/root comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     }
            :tx-listen (fn [tx-data root-cursor]
                         nil )}))






(defn comp-caret [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (dom/table #js {:className "caret-table"}
                             (dom/tbody nil
                                        (dom/tr nil
                                                (dom/td nil ":focus-path")
                                                (dom/td nil (str "[" (string/join " " (:focus-path data)) "]")))
                                        (dom/tr nil
                                                (dom/td nil ":focus-offset")
                                                (dom/td nil (:focus-offset data)))
                                        (dom/tr nil
                                                (dom/td nil ":anchor-path")
                                                (dom/td nil (str "[" (string/join " " (:anchor-path data)) "]")))
                                        (dom/tr nil
                                                (dom/td nil ":anchor-offset")
                                                (dom/td nil (:anchor-offset data))))))))

(om/root comp-caret data4
         {:target (. js/document (getElementById "caret"))
          :path [:caret]})
