(ns sthomp.om-richeditor.examples.bootstrap
  (:require 
    [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [cljs.core.async :refer [put! chan <! close!]]
    [sthomp.om-richeditor :as edtr]
    [figwheel.client :as fw :include-macros true]))

(def data4 (atom {:dom [{:tag "a" :text "Some Terminal Node2" :attrs {:href "http://www.google.com" :rel "nofollow" :target "_blank"}}
                        {:tag "span" :text "Hello world"}
                        {:tag "div" :children [{:tag "span" :text "Child1"}
                                             {:tag "span" :text "Child2"}
                                             {:tag "div" :children [{:tag "span" :text "GrandChild1"}
                                                                  {:tag "span" :text "GrandChild2"}
                                                                  {:tag "p" :text "GrandChild3"}
                                                                  {:tag "p" :children [{:tag "span" :text "GrandChild2"}
                                                                                       {:tag "span" :text "GrandChild3"}]}]}]}
                        {:tag "pre" :text "Another line..."}
                        {:tag "ul" :children [{:tag "li" :text "List Item1"}
                                              {:tag "li" :text "List Item2"}]}]
                  :caret {:focus-path []
                          :focus-offset 0
                          :anchor-offset 0
                          :anchor-path []
                          :is-collapsed true }}))


(let [click-chan (chan)]
  (om/root edtr/comp-richeditor data4
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     }
            :opts {:readonly false}
            :tx-listen (fn [tx-data root-cursor]
                         nil)}))


(om/root edtr/comp-caret data4
         {:target (. js/document (getElementById "caret"))
          :path [:caret]})

(om/root edtr/comp-richeditor data4
         {:target (. js/document (getElementById "viewer"))
          :opts {:readonly true}})


(fw/watch-and-reload  :jsload-callback (fn []
                                         ;; you would add this if you
                                         ;; have more than one file
                                         #_(reset! flap-state @flap-state)
                                         ))
