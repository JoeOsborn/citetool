(ns ^:figwheel-always citetool.ui.async-image
  (:require [citetool.ui.util :as u]
            [cljs.core.async :as async]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [citetool.ui.frame-provider :as fp])
  (:require-macros [cljs.core.async.macros :as async-m]))

(defn- -update-async-image! [owner]
  (async/put! (:control (om/get-state owner)) :changed))

(defn async-image [data owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [source (:source data)
            frame-chan (fp/frame-broadcast-channel source)
            control-chan (async/chan)]
        (async-m/go-loop
          [pc-id nil]
          (let [pc-id pc-id                                 ;silence intellij warnings
                prev-target (:now (om/get-props owner))
                [received-data chan] (async/alts! [control-chan frame-chan])
                cur-target (:now (om/get-props owner))
                priority (:priority (om/get-props owner))
                cur-frame (:frame (om/get-state owner))]
            #_(println received-data "prev target" prev-target "cur target" cur-target "cf" cur-frame)
            (cond
              (= chan control-chan) (case received-data
                                      :stop
                                      (do
                                        (when pc-id (fp/cancel-precache source pc-id))
                                        (async/close! control-chan)
                                        (async/close! frame-chan))
                                      :changed
                                      (if (and (= cur-target prev-target)
                                               (not= cur-frame nil))
                                        (recur pc-id)
                                        (let [response-chan (fp/request-frame-chan source cur-target priority)
                                              {stand-in :stand-in new-pc-id :precache-id} (async/<! response-chan)
                                              {image-data :image-data frame :frame} stand-in]
                                          (async/close! response-chan)
                                          (om/set-state! owner {:image-data image-data :frame frame :control control-chan})
                                          (when pc-id (fp/cancel-precache source pc-id))
                                          (recur new-pc-id))))
              (= cur-target cur-frame) (recur pc-id)
              (= chan frame-chan) (if-let [{frame :frame image-data :image-data} received-data]
                                    (cond
                                      (= frame cur-target) (do
                                                             (println "got frame" frame)
                                                             (om/set-state! owner {:image-data image-data
                                                                                   :frame      frame
                                                                                   :control    control-chan})
                                                             (recur nil))
                                      (u/closer? frame cur-target cur-frame) (do
                                                                               (println "updating standin from" cur-frame
                                                                                        "to" frame
                                                                                        "target" cur-target)
                                                                               (om/set-state! owner {:image-data image-data
                                                                                                     :frame      frame
                                                                                                     :control    control-chan})
                                                                               (recur pc-id))
                                      true (recur pc-id))
                                    (recur pc-id)))))
        {:image-data nil :frame nil :control control-chan}))
    om/IWillMount
    (will-mount [_]
      (-update-async-image! owner))
    om/IWillUnmount
    (will-unmount [_]
      (async/put! (:control (om/get-state owner)) :stop))
    om/IDidUpdate
    (did-update [_ _p-p _]
      (-update-async-image! owner))
    om/IRenderState
    (render-state [_ {image-data :image-data}]
      (dom/img (clj->js (merge {:src image-data :width (:width data) :height (:height data)}
                               (:attrs data)))))))


