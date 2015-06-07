(ns ^:figwheel-always citetool.ui.core
  (:require [figwheel.client :include-macros true]
            [cljs.core.async :as async]
            [clojure.browser.dom]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :as async-m]))

(enable-console-print!)

(declare on-js-reload)
(declare app-state)

(figwheel.client/watch-and-reload
  :websocket-url "ws://localhost:3449/figwheel-ws"
  :jsload-callback on-js-reload)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc))

;; define your app data so that it doesn't get over-written on reload

(defonce offscreen-canvas (clojure.browser.dom/html->dom "<canvas/>"))

(defn frame-image-data [frame]
  (let [w 320
        h 240
        f 96]
    (set! (.-width offscreen-canvas) w)
    (set! (.-height offscreen-canvas) h)
    (let [ctx (.getContext offscreen-canvas "2d")]
      #_(
      ctx.fillStyle = "rgb(255,255,255)";
      ctx.fillRect(0, 0, 64, 64);
      ctx.fillStyle = "rgb(0,0,0)";
      ctx.textAlign = "center"
      ctx.fillText((str frame),0,0,64)
      )
      (set! (.-fillStyle ctx) "rgb(200,200,200)")
      (.fillRect ctx 0 0 w h)
      (set! (.-textAlign ctx) "center")
      (set! (.-fillStyle ctx) "rgb(255,0,0)")
      (println "frame:" (str frame))
      (set! (.-font ctx) (str f "px serif"))
      (.fillText ctx (str frame) (/ w 2) (+ (/ h 2) (/ f 4)) w)
      (.toDataURL offscreen-canvas))))

(defn frame-image-provider []
  (let [requests (async/chan)
        responses (async/chan)]
    (async-m/go-loop
      []
      (let [request (async/<! requests)]
        (async/<! (async/timeout 1000))
        (if-let [{frame :frame} request]
          (async/>! responses {:frame frame, :image-data (frame-image-data frame)})
          (println "Unrecognized request " request))
        (recur)))
    {:out (async/pub responses :frame) :in requests}))

(defonce star-img "data:image/gif;base64,R0lGODlhEAAQAMQAAORHHOVSKudfOulrSOp3WOyDZu6QdvCchPGolfO0o/XBs/fNwfjZ0frl3/zy7////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAkAABAALAAAAAAQABAAAAVVICSOZGlCQAosJ6mu7fiyZeKqNKToQGDsM8hBADgUXoGAiqhSvp5QAnQKGIgUhwFUYLCVDFCrKUE1lBavAViFIDlTImbKC5Gm2hB0SlBCBMQiB0UjIQA7")

(defonce app-state (atom {:text "Hello!" :source (frame-image-provider) :frame 1}))

(defn request-frame [provider frame]
  (let [resp (async/chan)]
    (async-m/go
      (async/sub (:out provider) frame resp true)
      (async/>! (:in provider) {:frame frame}))
    {:channel resp :stand-in star-img}))

;(defprotocol IImgSource
;  "Produces AsyncImages on demand."
;  (request-frame [this frame]) ; -> {:channel channel , :stand-in image-url}
;  (preload-frames [this low high step))

(defn- update-async-image- [data owner]
  (let [{receipt :channel standin :stand-in} (request-frame (:source data) (:frame data))]
    (om/set-state! owner {:image-data standin})
    (async-m/go
      (let [state-data (async/<! receipt)]
        (om/set-state! owner state-data)
        (async/close! receipt)
        ))))

(defn async-image [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (println "MOUNT make query for " (:frame data))
      (update-async-image- data owner))
    om/IWillReceiveProps
    (will-receive-props [_ new-props]
      (println "RECV make query for " (:frame new-props))
      (when-not (= new-props (om/get-props owner))
        (update-async-image- new-props owner)))
    om/IRenderState
    (render-state [_ {image-data :image-data}]
      (dom/img #js {:src image-data :width 320 :height 240}))))

(om/root
  (fn [data _owner]
    (reify om/IRender
      (render [_]
        (om/build async-image data))))
  app-state
  {:target (.getElementById js/document "app")})
