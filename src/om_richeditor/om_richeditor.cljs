(ns sthomp.om-richeditor
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! close!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [goog.dom :as gdom]
            [goog.dom.Range :as grange]
            [goog.events :as gevents]))


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
                          :anchor-path []
                          :is-collapsed true }}))
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


(defn get-dom-caret []
  (let [selection (-> js/window .getSelection)]
    {:focusNode (.-focusNode selection)
     :anchorNode (.-anchorNode selection) 
     :focus-offset (.-focusOffset selection)
     :anchor-offset (.-anchorOffset selection)
     :is-collapsed (.-isCollapsed selection)}))




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


(defn fire-mouse-dom-event [elem event-type]
  "Fire an event of type event-type on the elem where elem is a dom node"
  (let [ev-obj (.createEvent js/document "MouseEvents")]
    (.initEvent ev-obj event-type true true)
    (.dispatchEvent elem ev-obj)))

(defn probe-for-caret []
  "Forces a click on the element where the caret is"
  (let [selection (.getSelection js/window)
        focus-elem (.-focusNode selection)
        anchor-elem (.-anchorNode selection)]
    (println "***Probe***")
    (.log js/console anchor-elem focus-elem (.-anchorOffset selection) (.-focusOffset selection))
    (if (.-isCollapsed selection)
      (fire-mouse-dom-event focus-elem "probe-collapsed-node")
      (do 
        (fire-mouse-dom-event anchor-elem "probe-anchor-node")
        (fire-mouse-dom-event focus-elem "probe-focus-node")))))

(defn keycode->char [keycode]
  (condp = keycode
    32 "\u00a0"
    (let [new-char (char keycode)]
      (if (string/blank? new-char)
        nil
        new-char))))

(defn notify-caret [data owner caret-chan caret-type]
  (let [dom-caret (get-dom-caret)]
    (println "***Notify***")
    (println "focus " (:focus-offset dom-caret))
    (println "anchor " (:anchor-offset dom-caret))
    (println "type " caret-type)
    (put! caret-chan {:path (om.core/path data )
                      :current-target (om/get-node owner)
                      :caret-type caret-type 
                      :dom-caret dom-caret }) 
    )
  )

(defn notify-caret-collapsed [data owner caret-chan]
  "Send a notification to update the collapse caret"
  (notify-caret data owner caret-chan :caret-collapsed))

(defn notify-caret-focus [data owner caret-chan]
  "Send a notification to update only the focus caret"
  (notify-caret data owner caret-chan :caret-focus))

(defn notify-caret-anchor [data owner caret-chan]
  "Send a notification to update only the anchor caret"
  (notify-caret data owner caret-chan :caret-anchor))

(defn comp-terminal-node [data owner {:keys [readonly] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      
      {:attrs (if readonly
                {}
                {:onMouseUp (fn [e]
                            (println "Terminal mouseup")
                            (.. e stopPropagation)   ;; prevent parent nodes from receiving the click event
                            (probe-for-caret)
                            ;; This timeout catches the case
                            ;; where the user clicks a current
                            ;; range selection
                            (js/setTimeout 
                              (fn [] 
                                (println "***Timeout***")
                                (probe-for-caret))
                              1))})})
    om/IWillMount
    (will-mount [_]
                )
    om/IDidMount
    (did-mount [this]
      (if-not readonly
        (if-let [caret-chan (om/get-shared owner :click-chan)]
          (do
            (gevents/listen (om/get-node owner) "probe-anchor-node" (fn [e] 
                                                                      (notify-caret-anchor data owner caret-chan)))
            (gevents/listen (om/get-node owner) "probe-focus-node" (fn [e] 
                                                                     (notify-caret-focus data owner caret-chan)))
            (gevents/listen (om/get-node owner) "probe-collapsed-node" (fn [e] 
                                                                         (println "probe-collapsed-node" (:is-collapsed (get-dom-caret)))
                                                                         (notify-caret-collapsed data owner caret-chan))))
          (throw (js/Error. (str "You must create a :click-chan in shared state")))) )
      )
    om/IRenderState
    (render-state [this state]
                  (let [{:keys [attrs]} (om/get-state owner)]
                    (json-terminal-node->om-node data attrs)))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                )))

(defn comp-node [data owner {:keys [readonly] :as opts}]
  (reify
    om/IRenderState
    (render-state [this state]
                  (if (contains? data :children)
                    (do
                      (:tag data)   ;; TODO: Need to use the tag as the node
                      (apply dom/pre nil
                             (om/build-all comp-node (:children data) {:opts opts})))
                    (do
                      (om/build comp-terminal-node data {:opts opts}))))))

;; Keycodes
(def BACKSPACE 8)
(def DELETE 46)
(def LEFTARROW 37)
(def RIGHTARROW 39)
(def UPARROW 38)
(def DOWNARROW 40)





(defn move-caret-offset [n root-data]
  "Move the caret forward/back by n characters"
  (om/transact! root-data [:caret]
                (fn [caret]
                  (let [{:keys [focus-offset anchor-offset]} caret
                        new-offset (+ focus-offset n)]
                    (assoc caret
                      :focus-offset new-offset
                      :anchor-offset new-offset)))))

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

(defn string-length-diff [str1 str2]
  (- (count str2) (count str1)))

(defn get-text-value [root-data path]
  (get-in @root-data (conj path :text)))

(defn handle-keypress [e root-data]
  (let [caret (-> @root-data :caret)
        {:keys [focus-path focus-offset anchor-offset]} caret]
    (condp = (.. e -type)
      "keydown" (condp = (.. e -which)
                  ;; TODO: Delete the node from the data model if its length is 0
                  BACKSPACE (do
                              (.. e preventDefault)
                              (let [current-value (get-text-value root-data focus-path)
                                    new-value (str (subs current-value 0 (dec focus-offset)) (subs current-value focus-offset))
                                    char-diff (string-length-diff current-value new-value)]
                                (om/transact! root-data (conj focus-path :text)
                                              (fn [text]
                                                new-value))
                                (move-caret-offset char-diff root-data)))
                  DELETE (do
                           (.. e preventDefault)
                           (let [current-value (get-text-value root-data focus-path)
                                 new-value (str (subs current-value 0 focus-offset) (subs current-value (inc focus-offset)))
                                 char-diff (string-length-diff current-value new-value)]
                             (om/transact! root-data (conj focus-path :text)
                                           (fn [text]
                                             new-value)))
                           (println "delete"))
                  nil)
      "keyup" (condp = (.. e -which)
                ;; Block certain keystrokes from propogating to keypress
                ;; but let regular keystrokes pass through (like pressing a character)
                UPARROW (do
                          (probe-for-caret)
                          )
                DOWNARROW (do
                            (probe-for-caret)
                            )
                LEFTARROW (do
                            (probe-for-caret)
                            )
                RIGHTARROW (do
                             (probe-for-caret)
                             )
                nil)
      "keypress" (let [keycode (.. e -which)
                       new-char (keycode->char keycode)]
                   (.. e preventDefault)
                   (let [current-value (get-text-value root-data focus-path)
                         new-value (str (subs current-value 0 focus-offset) new-char (subs current-value focus-offset))
                         char-diff (string-length-diff current-value new-value)]
                     (println "|" (count (str "a" "" "b")) "|")
                     (om/transact! root-data (conj focus-path :text)
                                   (fn [text]
                                     new-value))
                     (println "Move caret by " char-diff " chars")
                     (move-caret-offset char-diff root-data))
                   (println "keypress"))
      nil)))

(defn update-collapsed-caret [app path terminal-dom-node dom-caret]
  (let [ dom-node-caret (.. (:focusNode dom-caret) -parentNode)
        new-caret (if (identical? terminal-dom-node dom-node-caret)
                    {:focus-offset (:focus-offset dom-caret)
                     :anchor-offset (:anchor-offset dom-caret)
                     :focus-path path
                     :anchor-path path
                     :is-collapsed (:is-collapsed dom-caret)}
                    {:focus-offset 0
                     :anchor-offset 0
                     :focus-path path
                     :anchor-path path
                     :is-collapsed (:is-collapsed dom-caret)})]
    (om/update! app :caret new-caret)))

(defn update-focus-caret [app path dom-caret]
  (println "***Update Focus***")
  (om/transact! app
                :caret
                #(conj % {:focus-offset (:focus-offset dom-caret) :focus-path path :is-collapsed false})))

(defn update-anchor-caret [app path dom-caret]
  (println "***Update Anchor***")
  (om/transact! app
                :caret
                #(conj % {:anchor-offset (:anchor-offset dom-caret) :anchor-path path :is-collapsed false})))

(defn focus-before-anchor? [{:keys [focus-path focus-offset anchor-path anchor-offset] :as caret}]
  (cond
    (and (= focus-path anchor-path) (= focus-offset anchor-offset)) 0
    (and (= focus-path anchor-path) (< focus-offset anchor-offset)) true
    (and (= focus-path anchor-path) (> focus-offset anchor-offset)) false
    :else false
    )
  )

(defn comp-richeditor [data owner {:keys [readonly] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      {:attrs (if readonly
                #js {}
                #js {:contentEditable true
                     :spellCheck false
                     :onMouseUp (fn [e] 
                                  (println "Richeditor mouseup")
                                  (probe-for-caret))
                     :onKeyDown (fn [e]
                                  (handle-keypress e data))
                     :onKeyUp (fn [e]
                                (handle-keypress e data))
                     :onKeyPress (fn [e]
                                   (handle-keypress e data))
                     :onFocus (fn [e]
                                (om/set-state! owner :focused true))
                     :onBlur (fn [e]
                               (om/set-state! owner :focused false))})
       :focused false})
    om/IWillMount
    (will-mount [_]
                (when-let [click-chan (om/get-shared owner :click-chan)]
                  ;; Listen for clicks on terminal nodes so we can reset the caret
                  (go (loop []
                        (let [{:keys [path current-target caret-type dom-caret]} (<! click-chan)]
                          (condp = caret-type
                            :caret-collapsed (update-collapsed-caret data path current-target dom-caret)
                            :caret-focus (update-focus-caret data path dom-caret)
                            :caret-anchor (update-anchor-caret data path dom-caret)
                            (throw (js/Error. (str "Unknown caret-type" caret-type)))))
                        (recur)))))
    om/IRenderState
    (render-state [this state]
                  (if-let [attrs (:attrs (om/get-state owner))]
                    (dom/div nil
                             (apply dom/div attrs
                                    (om/build-all comp-node (:dom data) {:opts opts})))
                    (throw (js/Error. (str "You must create a :attrs in local state")))))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      ;; Update the caret location

      (println "***Rerender***")
      (when (om/get-state owner :focused)
        (let [focus-path (-> data :caret :focus-path)
              anchor-path (-> data :caret :anchor-path)
              focus-dom-node (path->dom-node owner focus-path)
              anchor-dom-node (path->dom-node owner anchor-path)
              focus-offset (-> data :caret :focus-offset)
              anchor-offset (-> data :caret :anchor-offset)]
          (.log js/console anchor-dom-node anchor-offset focus-dom-node focus-offset)
          (.select (grange/createFromNodes (.-firstChild anchor-dom-node) 
                                           anchor-offset
                                           (.-firstChild focus-dom-node)
                                           focus-offset)))))))






;; Component to view the caret


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
                                                (dom/td nil (:anchor-offset data)))
                                        (dom/tr nil
                                                (dom/td nil ":is-collapsed")
                                                (dom/td nil (str (:is-collapsed data)))))))))

;; Om Roots

(let [click-chan (chan)]
  (om/root comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     }
            :opts {:readonly false}
            :tx-listen (fn [tx-data root-cursor]
                         nil)}))


(om/root comp-caret data4
         {:target (. js/document (getElementById "caret"))
          :path [:caret]})

(om/root comp-richeditor data4
         {:target (. js/document (getElementById "viewer"))
          :opts {:readonly true}})

#_(js/setTimeout 
  (fn [] 
    (println "timeout")
    ;; document.getElementById("viewer").firstChild.firstChild.firstChild.focus()
    (.. js/document (getElementById "viewer") -firstChild -firstChild -firstChild focus))
  3000)
