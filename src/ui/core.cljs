(ns ^:figwheel-always citetool.ui.core
    (:require [figwheel.client :as fw :include-macros true]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)

(declare on-js-reload)
(declare app-state)

(fw/watch-and-reload
  :websocket-url   "ws://localhost:3449/figwheel-ws"
  :jsload-callback on-js-reload)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello!"}))

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (dom/h1 nil (:text data)))))
  app-state
  {:target (. js/document (getElementById "app"))})
