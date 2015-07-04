(ns ^:figwheel-always citetool.ui.async-image
  (:require [citetool.ui.util :as u]
            [cljs.core.async :as async]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [citetool.ui.frame-provider :as fp])
  (:require-macros [cljs.core.async.macros :as async-m]))

(defn- -update-async-image [data owner]
  (let [{receipt :channel standin :stand-in} (fp/request-frame (:source data) (:now data))
        old-receipt (:receipt (om/get-state owner))]
    (when old-receipt
      (async/close! old-receipt))
    (om/set-state! owner {:image-data standin :frame (:now data) :receipt receipt})
    (async-m/go
      (let [state-data (async/<! receipt)]
        (when (and state-data (= (:now (om/get-props owner)) (:now data)))
          (om/set-state! owner state-data))
        (async/close! receipt)))))

(defn async-image [data owner opts]
  (reify
    om/IWillMount
    (will-mount [_]
      (println "MOUNT make query for " (:now data))
      (-update-async-image data owner))
    om/IWillReceiveProps
    (will-receive-props [_ new-props]
      (when (or (not= (:now new-props) (:now (om/get-props owner)))
                (not= (:source new-props) (:source (om/get-props owner))))
        (println "RECV make query for" (:now new-props) "vs" (:now (om/get-props owner)))
        (-update-async-image new-props owner)))
    om/IRenderState
    (render-state [_ {image-data :image-data}]
      (dom/img (clj->js (merge {:src image-data :width (:width data) :height (:height data)}
                               opts))))))


