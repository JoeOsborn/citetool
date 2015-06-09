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
  (swap! app-state update-in [:__figwheel_counter] inc))

(defonce offscreen-canvas (clojure.browser.dom/html->dom "<canvas/>"))

(defn frame-image-data [frame]
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
        (async/<! (async/timeout 500))
        (if-let [{frame :frame} request]
          (async/>! responses {:frame frame, :image-data (frame-image-data frame)})
          (println "Unrecognized request " request))
        (recur)))
    {:out (async/pub responses :frame) :in requests}))

(defonce
  star-img
  (str "data:image/gif;base64,R0lGODlhEAAQAMQAAORHHOVSKudfOulrSOp3WOyDZu6QdvCchPGolfO0o/XBs/fNwfjZ0f"
       "rl3/zy7////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAkAABAALAAAAAAQA"
       "BAAAAVVICSOZGlCQAosJ6mu7fiyZeKqNKToQGDsM8hBADgUXoGAiqhSvp5QAnQKGIgUhwFUYLCVDFCrKUE1lBav"
       "AViFIDlTImbKC5Gm2hB0SlBCBMQiB0UjIQA7"))

; Provider should be an {:out pub :in chan} map
(defn request-frame [provider frame]
  (let [resp (async/chan)]
    (async/sub (:out provider) frame resp true)
    (async/put! (:in provider) {:frame frame})
    {:channel resp :stand-in star-img}))

(defn- -update-async-image [data owner]
  (let [{receipt :channel standin :stand-in} (request-frame (:source data) (:now data))
        old-receipt (:receipt (om/get-state owner))]
    (when old-receipt
      (async/close! old-receipt))
    (om/set-state! owner {:image-data standin :frame (:now data) :receipt receipt})
    (async-m/go
      (let [state-data (async/<! receipt)]
        (when (and state-data (= (:now (om/get-props owner)) (:now data)))
          (om/set-state! owner state-data))
        (async/close! receipt)
        ))))

(defn async-image [data owner]
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
      (dom/img (clj->js {:src   image-data :width 640 :height 480
                         :style {:margin-left 80}})))))

(defn floor [a] (.floor js/Math a))

(defn clip [lo x hi]
  (min hi (max lo x)))

(defn frame-offset-x [frame scroll-width context]
  (let [frames-per-pixel (/ scroll-width context)]
    (floor (+ 2 (* frames-per-pixel frame)))))

(defn inv-frame-offset-x [x scroll-width context duration]
  (let [frames-per-pixel (/ scroll-width context)]
    (clip 0 (floor (/ x frames-per-pixel)) (dec duration))))

(defn shown-tick-frames [w context left-x duration]
  (let [frame-skip 32
        left-frame (inv-frame-offset-x left-x w context duration)
        left-frame (- left-frame (mod left-frame frame-skip))
        right-frame (+ left-frame context frame-skip)
        last-frame (dec duration)
        frames-maybe-missing-end (range left-frame (min right-frame last-frame) frame-skip)
        frames (if (< right-frame last-frame)
                 frames-maybe-missing-end
                 (concat frames-maybe-missing-end [last-frame]))]
    frames))

(defn jump-to-frame! [frame]
  (println "jump to " frame)
  (swap! app-state update-in [:now] (fn [_] frame))
  ;jump scroll-x
  )

; Do this goofy declare/remove/define/add dance to make sure we don't put two
; event handlers on the document.
(declare handle-keyboard!)
(.removeEventListener js/document "keydown" handle-keyboard!)
(defn handle-keyboard! [e]
  (println "K:" (.-keyCode e) "shift:" (.-shiftKey e))
  (let [now (:now @app-state)
        duration (:duration @app-state)
        left-x (get-in @app-state [:timeline :scroll-x])
        scroll-width (:scroll-width @app-state)
        shown-ticks (shown-tick-frames scroll-width (:context (:timeline @app-state)) left-x duration)
        nearest-tick-left (or (last (filter #(< % now) shown-ticks)) 0)
        nearest-tick-right (or (first (filter #(> % now) shown-ticks)) (dec duration))]
    (case (.-keyCode e)
      38 true                                               ; up
      40 true                                               ; down
      37 (jump-to-frame! (max 0                             ; left
                              (if (.-shiftKey e)
                                nearest-tick-left
                                (dec now))))
      39 (jump-to-frame! (min (dec duration)                ; right
                              (if (.-shiftKey e)
                                nearest-tick-right
                                (inc now))))
      true)
    (.preventDefault e)))
(.addEventListener js/document "keydown" handle-keyboard!)

(defn tick-mark [data _owner {w :width}]
  (reify
    om/IRender
    (render [_]
      (dom/div (clj->js {:style {:border         "1px solid grey"
                                 :width          "0px"
                                 :height         "96%"
                                 :position       :absolute
                                 :left           (+ 6 (frame-offset-x (:frame data) w (:context data)))
                                 :top            0
                                 :pointer-events "none"}})))))

(defn timeline [data owner {w :width h :height y :y}]
  (reify
    om/IDidMount
    (did-mount [_]
      (om/transact! (:timeline data)
                    [:scroll-x]
                    (fn [_] (.-scrollLeft (om/get-node owner)))))
    om/IDidUpdate
    (did-update [_ _ _]
      (set! (.-scrollLeft (om/get-node owner)) (get-in data [:timeline :scroll-x])))
    om/IRender
    (render [_]
      (let [context (:context (:timeline data))
            scroll-x (:scroll-x (:timeline data))
            now (:now data)
            duration (:duration data)]
        (dom/div
          (clj->js {:style    {:overflow-x "scroll"
                               :position   :absolute
                               :left       0
                               :top        (str y "px")
                               :width      (str w "px")
                               :height     (str h "px")}
                    :onScroll (fn [_e]
                                (om/transact! (:timeline data)
                                              [:scroll-x]
                                              (fn [_] (.-scrollLeft (om/get-node owner)))))})
          (dom/div (clj->js {:style   {:backgroundColor     "rgb(50,50,200)"
                                       :border              "16px solid green"
                                       :border-left-width   "8px"
                                       :border-right-width  "8px"
                                       :border-bottom-width "0px"
                                       :width               (str (* (/ w context) duration) "px")
                                       :height              (str (- h 31) "px")}
                             :onClick (fn [e]
                                        (let [mx (.-pageX e)
                                              scroll-box (om/get-node owner)
                                              ol (.-scrollLeft scroll-box)
                                              mx (+ mx ol)
                                              mx (- mx 8)
                                              ctx (:context @(:timeline data))
                                              dur (:duration data)]
                                          (jump-to-frame! (inv-frame-offset-x mx w ctx dur))))}))
          (apply dom/div nil (om/build-all tick-mark
                                           (map (fn [f] {:frame f :context context})
                                                (shown-tick-frames w
                                                                   context
                                                                   scroll-x
                                                                   duration))
                                           {:opts {:width w}}))
          (dom/div (clj->js {:style {:border         "4px solid red"
                                     :width          "4px"
                                     :height         "90%"
                                     :position       :absolute
                                     :pointer-events "none"
                                     :left           (+ 1 (frame-offset-x now w context))
                                     :top            0}})))))))

(def app-state (atom {:source       (frame-image-provider)
                      :metadata     {}
                      :now          0
                      :timeline     {:context  1000
                                     :scroll-x 0}
                      :scroll-width 800
                      :annotations  []
                      :edits        []
                      :duration     2000}))

(om/root
  (fn [data _owner]
    (reify om/IRender
      (render [_]
        (dom/div {}
                 (om/build async-image data)
                 (om/build timeline
                           {:timeline (:timeline data)
                            :now      (:now data)
                            :edits    (:edits data)
                            :duration (:duration data)}
                           {:opts {:width (:scroll-width data) :height 100 :y 478}})))))
  app-state
  {:target (.getElementById js/document "app")})

;(.setTimeout js/window (fn [] (swap! app-state update-in [:frame] inc)) 2000)