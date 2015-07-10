(ns ^:figwheel-always citetool.ui.async-image
  (:require [citetool.ui.util :as u]
            [cljs.core.async :as async]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [citetool.ui.frame-provider :as fp])
  (:require-macros [cljs.core.async.macros :as async-m]))

(defn -cancel-pending-request! [data owner]
  (let [old-receipt (:receipt (om/get-state owner))
        old-pc-id (:precache-id (om/get-state owner))]
    (when old-receipt
      (println "CANCEL" old-pc-id)
      ;cancel and close channel
      (fp/cancel-precache (:source data) old-pc-id)
      (async/put! old-receipt :cancel))))

(defn- -update-async-image! [data owner]
  (-cancel-pending-request! data owner)
  (fp/request-frame
    (:source data) (:now data)
    (fn [{receipt :channel standin :stand-in pc-id :precache-id}]
      (println "in callback with" (:frame standin) pc-id)
      (om/set-state! owner {:image-data  (:image-data standin)
                            :frame       (:frame standin)
                            :precache-id pc-id
                            :receipt     receipt})
      (let [standin-frame (:frame (om/get-state owner))
            target-frame (:now data)]
        (async-m/go-loop []
                         (let [received-data (async/<! receipt)]
                           (if (or (= received-data :cancel)
                                   (not= receipt (:receipt (om/get-state owner)))
                                   (not= target-frame (:now (om/get-props owner))))
                             (async/close! receipt)
                             (if-let [{frame :frame image-data :image-data} received-data]
                               (do
                                 (println "received" frame)
                                 (cond
                                   (= frame target-frame) (do
                                                            (println "got frame" frame)
                                                            (om/set-state! owner {:image-data  image-data
                                                                                  :frame       frame
                                                                                  :precache-id nil
                                                                                  :receipt     receipt})
                                                            (async/close! receipt))
                                   (u/closer? frame target-frame standin-frame) (do
                                                                                  (println "updating standin from" standin-frame
                                                                                           "to" frame
                                                                                           "target" target-frame)
                                                                                  (om/set-state! owner {:image-data  image-data
                                                                                                        :frame       frame
                                                                                                        :precache-id pc-id
                                                                                                        :receipt     receipt})
                                                                                  (recur))
                                   true (recur)))
                               (recur)))))))))

(defn async-image [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:image-data nil :frame nil :receipt nil})
    om/IWillMount
    (will-mount [_]
      (println "MOUNT make query for " (:now data))
      (-update-async-image! data owner))
    om/IWillUnmount
    (will-unmount [_]
      (-cancel-pending-request! data owner))
    om/IWillReceiveProps
    (will-receive-props [_ new-props]
      (when (or (not= (:now new-props) (:now (om/get-props owner)))
                (not= (:source new-props) (:source (om/get-props owner))))
        (println "RECV make query for" (:now new-props) "vs" (:now (om/get-props owner)))
        (-update-async-image! new-props owner)))
    om/IRenderState
    (render-state [_ {image-data :image-data}]
      (dom/img (clj->js (merge {:src image-data :width (:width data) :height (:height data)}
                               (:attrs data)))))))


