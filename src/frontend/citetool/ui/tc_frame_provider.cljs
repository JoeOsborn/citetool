(ns ^:figwheel-always citetool.ui.tc-frame-provider
  (:require [clojure.browser.dom]
            [citetool.ui.util :as u]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :as async-m]))

(defonce offscreen-canvas (clojure.browser.dom/html->dom "<canvas/>"))

(defn frame-image-data [frame duration]
  (let [w 320
        h 240
        f 96]
    (set! (.-width offscreen-canvas) w)
    (set! (.-height offscreen-canvas) h)
    (let [ctx (.getContext offscreen-canvas "2d")]
      (set! (.-fillStyle ctx) "rgb(200,200,200)")
      (.fillRect ctx 0 0 w h)
      (set! (.-textAlign ctx) "center")
      (set! (.-fillStyle ctx) "rgb(255,0,0)")
      (println "draw frame:" (str frame))
      (set! (.-font ctx) (str f "px serif"))
      (.fillText ctx (str "fm " frame) (/ w 2) (+ (/ h 4) (/ f 4)) w)
      (.fillText ctx (str (u/frame->timecode frame duration)) (/ w 2) (+ (* 3 (/ h 4)) (/ f 4)) w)
      (.toDataURL offscreen-canvas))))

(defn frame-image-provider [duration]
  (let [requests (async/chan)
        responses (async/chan)
        fake-ms-per-frame 1]
    (async-m/go-loop
      [last-frame 0]
      (let [request (async/<! requests)]
        (if-let [{frame :frame} request]
          (do
            (async/<! (async/timeout (if (>= frame last-frame)
                                       ;simulate running from last-frame to frame
                                       (* fake-ms-per-frame (- frame last-frame))
                                       ;otherwise simulate running from 0
                                       (* fake-ms-per-frame frame))))
            (async/>! responses {:frame frame, :image-data (frame-image-data frame duration)})
            (recur frame))
          (do
            (println "Unrecognized request " request)
            (recur last-frame)))))
    {:out (async/pub responses :frame) :in requests}))
