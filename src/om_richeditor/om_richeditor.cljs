(ns sthomp.om-richeditor
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :refer [put! chan <! close!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [goog.dom :as gdom]
            [goog.dom.Range :as grange]
            [goog.events :as gevents])
  (:require-macros [cljs.core.async.macros :refer [go]])
  )

;; [sthomp.om-richeditor :as macros]
(enable-console-print!)

(def not-nil? (complement nil?))

(defn parse-hiccup-node [node]
  (assert (sequential? node) (str "A hiccup node must sequential: " node))
  (assert (keyword? (first node)) (str "A hiccup node must start with a keyword: " node))
  (let [[tag & tail] node
        [attrs content content-idx] (if (map? (first tail))
                                      [(first tail) (next tail) 2]
                                      [{} tail 1])]
    (cond
      ;; Empty node
      (or (nil? content)
          (empty? content)) [tag attrs "" content-idx]
      ;; String node
      (and (= 1 (count content))
           (string? (first content))) [tag attrs (first content) content-idx]
      :else [tag attrs content content-idx])))

(defn remove-node [root-data path]
  {:pre [(> (count path) 0)]}
  (let [parent-path (subvec path 0 (dec (count path)))
        parent-elem (get-in root-data parent-path)
        elem (get-in root-data path)
        index-to-remove (last path)]
    (assert (vector? parent-elem))
    (last path)
    (vec (concat (subvec parent-elem 0 (dec index-to-remove)) (subvec parent-elem index-to-remove (count parent-elem))))
    ))

(defn get-dom-caret []
  (let [selection (-> js/window .getSelection)]
    {:focusNode (.-focusNode selection)
     :anchorNode (.-anchorNode selection) 
     :focus-offset (.-focusOffset selection)
     :anchor-offset (.-anchorOffset selection)
     :is-collapsed (.-isCollapsed selection)}))



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
    (if (.-isCollapsed selection)
      (do
        (fire-mouse-dom-event focus-elem "probe-collapsed-node"))
      (do 
        (fire-mouse-dom-event anchor-elem "probe-anchor-node")
        (fire-mouse-dom-event focus-elem "probe-focus-node")))))


(defn notify-caret [data owner caret-chan caret-type]
  (let [dom-caret (get-dom-caret)]
    (put! caret-chan {:path (om.core/path data )
                      :current-target (om/get-node owner)
                      :caret-type caret-type 
                      :dom-caret dom-caret
                      :text-node (om/get-node owner "text-node")}) 
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
                              1))
                 :ref "text-node"}
                )})
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
      (let [[tag attrs content _] (parse-hiccup-node data)
            local-attrs (:attrs state)
            merged-attrs (merge attrs local-attrs)]
        (assert (or (nil? content) (string? content)) "The content of terminal nodes can only be nil or a string")
        (html [tag merged-attrs content])
        ))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
                )))



(defn comp-node [data owner {:keys [readonly] :as opts}]
  (reify
    om/IRenderState
    (render-state [this state]
      ;; Has children
      (let [[tag attrs content _] (parse-hiccup-node data)]
        (if (sequential? content)
          (do
            ;; Has children
            (html
              [tag
               (map-indexed #(om/build comp-node %2 {:opts opts :react-key %1}) content)
               ]))
          ;; No children
          (do 
            (om/build comp-terminal-node data {:opts opts})
            ))))))

;; Keycodes
(def BACKSPACE 8)
(def DELETE 46)
(def LEFTARROW 37)
(def RIGHTARROW 39)
(def UPARROW 38)
(def DOWNARROW 40)
(def SPACEBAR 32)




(defn move-caret-offset [n caret]
  "Move the caret forward/back by n characters"
  (let [{:keys [focus-offset anchor-offset]} caret
        new-offset (+ focus-offset n)]
    (assoc caret
           :focus-offset new-offset
           :anchor-offset new-offset)))

(defn string-length-diff [str1 str2]
  (- (count str2) (count str1)))

(defn is-text-valid? [text] 
  (if (re-find #"\s{2,}" text)
    false
    true))


(defn handle-keypress [e root-data]
  (condp = (.. e -type)
    "keydown" (condp = (.. e -which)
                ;; TODO: Delete the node from the data model if its length is 0
                BACKSPACE (do
                            (.. e preventDefault)
                            (om/transact! root-data 
                                          (fn [data]
                                            (let [caret (:caret data)
                                                  {:keys [focus-path focus-offset anchor-path anchor-offset]} caret
                                                  anchor-node (get-in data anchor-path)
                                                  [_ _ _ content-idx] (parse-hiccup-node anchor-node)
                                                  text-path (conj focus-path content-idx)
                                                  current-value (get-in data text-path)
                                                  new-value (str (subs current-value 0 (dec focus-offset)) (subs current-value focus-offset))
                                                  char-diff (string-length-diff current-value new-value)
                                                  new-caret (move-caret-offset char-diff caret)]
                                              (-> data
                                                  (assoc-in text-path new-value)
                                                  (assoc :caret new-caret))))))
                DELETE (do
                         (.. e preventDefault)
                         (om/transact! root-data
                                       (fn [data]
                                         (let [caret (:caret data)
                                               {:keys [focus-path focus-offset anchor-path anchor-offset]} caret
                                               anchor-node (get-in data anchor-path)
                                               [_ _ _ content-idx] (parse-hiccup-node anchor-node)
                                               text-path (conj focus-path content-idx)
                                               current-value (get-in data text-path)
                                               new-value (str (subs current-value 0 focus-offset) (subs current-value (inc focus-offset)))]
                                           (assoc-in data text-path new-value))))) 
                nil)
    "keyup" (condp = (.. e -which)
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
                     new-char (char keycode)]
                 (.. e preventDefault)
                 (om/transact! root-data
                               (fn [data]
                                 (let [caret (:caret data)
                                       {:keys [focus-path focus-offset anchor-path anchor-offset]} caret
                                       anchor-node (get-in data anchor-path)
                                       [_ _ _ content-idx] (parse-hiccup-node anchor-node)
                                       text-path (conj focus-path content-idx)
                                       current-value (get-in data text-path)
                                       new-value (str (subs current-value 0 focus-offset) new-char (subs current-value focus-offset))
                                       char-diff (string-length-diff current-value new-value)
                                       new-caret (move-caret-offset char-diff (:caret data))]
                                   (if (is-text-valid? new-value)
                                     (-> data
                                         (assoc-in text-path new-value)
                                         (assoc :caret new-caret))
                                     (if (and (= keycode SPACEBAR)
                                              (or 
                                                (= \space (.charAt current-value focus-offset))
                                                (= \u00a0 (.charAt current-value focus-offset))))
                                       (assoc data :caret (move-caret-offset 1 (:caret data)))
                                       data))))))
    nil))

(defn update-collapsed-caret [app path terminal-dom-node dom-caret text-node]
  (let [ dom-node-caret (.. (:focusNode dom-caret) -parentNode)
        new-caret (if (identical? terminal-dom-node dom-node-caret)
                    {:focus-offset (:focus-offset dom-caret)
                     :anchor-offset (:anchor-offset dom-caret)
                     :focus-path path
                     :anchor-path path
                     :focus-text-node text-node
                     :anchor-text-node text-node
                     :is-collapsed (:is-collapsed dom-caret)}
                    {:focus-offset 0
                     :anchor-offset 0
                     :focus-path path
                     :anchor-path path
                     :focus-text-node text-node
                     :anchor-text-node text-node
                     :is-collapsed (:is-collapsed dom-caret)})]
    (println "updating caret " new-caret)
    (om/update! app :caret new-caret)))

(defn update-focus-caret [app path dom-caret text-node]
  (println "***Update Focus***")
  (om/transact! app
                :caret
                #(conj % {:focus-offset (:focus-offset dom-caret) :focus-path path :is-collapsed false :focus-text-node text-node})))

(defn update-anchor-caret [app path dom-caret text-node]
  (println "***Update Anchor***")
  (om/transact! app
                :caret
                #(conj % {:anchor-offset (:anchor-offset dom-caret) :anchor-path path :is-collapsed false :anchor-text-node text-node})))

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
                                ;; TODO: probe for caret when focused
                                (om/set-state! owner :focused true))
                     :onBlur (fn [e]
                               (om/set-state! owner :focused false))})
       :focused false})
    om/IWillMount
    (will-mount [_]
                (when-let [click-chan (om/get-shared owner :click-chan)]
                  ;; Listen for clicks on terminal nodes so we can reset the caret
                  (go (loop []
                        (let [{:keys [path current-target caret-type dom-caret text-node]} (<! click-chan)]
                          (condp = caret-type
                            :caret-collapsed (update-collapsed-caret data path current-target dom-caret text-node)
                            :caret-focus (update-focus-caret data path dom-caret text-node)
                            :caret-anchor (update-anchor-caret data path dom-caret text-node)
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
      (when (om/get-state owner :focused)
        (let [focus-path (-> data :caret :focus-path)
              anchor-path (-> data :caret :anchor-path)
              focus-offset (-> data :caret :focus-offset)
              anchor-offset (-> data :caret :anchor-offset)
              focus-text-node (-> data :caret :focus-text-node)
              anchor-text-node (-> data :caret :anchor-text-node)
              ]
          (assert (and (not= nil anchor-text-node) (not= nil focus-text-node)) "The caret cannot contain nil text nodes")
          (when (not-nil? anchor-text-node)
            (.select (grange/createFromNodes (.-firstChild anchor-text-node) 
                                             anchor-offset
                                             (.-firstChild focus-text-node)
                                             focus-offset))
            )
          )))))






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
                                                (dom/td nil (str (:is-collapsed data))))
                                        (dom/tr nil
                                                (dom/td nil ":anchor-text-node")
                                                (dom/td nil (str (:anchor-text-node data))))
                                        (dom/tr nil
                                                (dom/td nil ":focus-text-node")
                                                (dom/td nil (str (:focus-text-node data)))))))))

;; Om Roots

#_(js/setTimeout 
  (fn [] 
    (println "timeout")
    ;; document.getElementById("viewer").firstChild.firstChild.firstChild.focus()
    (.. js/document (getElementById "viewer") -firstChild -firstChild -firstChild focus))
  3000)



