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

(def data5 (atom {:dom [
                        [:pre#scott {} 
                         [:span.test {} "hello1"] 
                         [:span "hello2"]
                         [:div 
                          [:p {:class-name "hah"} "blah blah blah"]]]
                        ]
                  :caret {:focus-path []
                          :focus-offset 0
                          :anchor-offset 0
                          :anchor-path []
                          :is-collapsed true
                          :focus-text-node nil
                          :anchor-text-node nil}}))

(defn run-tests []
  #_(edtr/remove-node data4 [1])
  (println (edtr/hiccup-has-children? [:div]))
  (println (edtr/hiccup-has-children? [:div "hello1"]))
  (println (edtr/hiccup-has-children? [:div {:class "aya"} "hello1"]))
  (println (edtr/hiccup-has-children? [:div "test1" "test2" [:pre "pre"] "test3"]))
  (println (edtr/hiccup-has-children? [:div {:class "gig"}]))
  (println (edtr/hiccup-has-children? [:div {:class "ta"} [:span "hey"] [:pre {:class "h"} "har"]]))
  
  )


#_(run-tests)

;; om roots

(let [click-chan (chan)]
  (om/root edtr/comp-richeditor data5
           {:target (. js/document (getElementById "editor"))
            :shared {:click-chan click-chan   ;; Notifies the richeditor when clicks happen on a terminal node
                     }
            :opts {:readonly false}
            :tx-listen (fn [tx-data root-cursor]
                         nil)}))


(om/root edtr/comp-caret data5
         {:target (. js/document (getElementById "caret"))
          :path [:caret]})

#_(om/root edtr/comp-richeditor data5
         {:target (. js/document (getElementById "viewer"))
          :opts {:readonly true}})


#_(fw/watch-and-reload  :jsload-callback (fn []
                                         ;; you would add this if you
                                         ;; have more than one file
                                         #_(reset! flap-state @flap-state)
                                         ))
