(ns sthomp.om-richeditor.examples.bootstrap
  (:require 
    [sthomp.om-richeditor :as richeditor]
    [figwheel.client :as fw :include-macros true]))

(fw/watch-and-reload  :jsload-callback (fn []
                                         ;; you would add this if you
                                         ;; have more than one file
                                         #_(reset! flap-state @flap-state)
                                         ))