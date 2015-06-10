(ns ^:figwheel-always citetool.ui.core
  (:require [figwheel.client :include-macros true]
            [cljs.core.async :as async]
            [clojure.browser.dom]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.string :as gstring])
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

(defn floor [a] (.floor js/Math a))

(defn clip [lo x hi]
  (min hi (max lo x)))

(defn prev-item [item items]
  (or (last (filter #(< % item) items)) (first items)))

(defn next-item [item items]
  (or (first (filter #(> % item) items)) (last items)))

(defn prev-or-eq-item [item items]
  (or (last (filter #(<= % item) items)) (first items)))

(defn next-or-eq-item [item items]
  (or (first (filter #(>= % item) items)) (last items)))

(defn pad-right [s filler min-len]
  (if (>= (.-length s) min-len)
    s
    (recur (str s filler) filler min-len)))

(defn pad-left [s filler min-len]
  (if (>= (.-length s) min-len)
    s
    (recur (str filler s) filler min-len)))

(defn frame->timecode [frame pad-units]
  (let [h (floor (/ frame 108000))
        frame (- frame (* h 108000))
        m (floor (/ frame 1800))
        frame (- frame (* m 1800))
        s (floor (/ frame 30))
        frame (- frame (* s 30))
        ; frame to millisecond = (seconds/frame) * frame * (milliseconds/second)
        millis (floor (* (/ 1 30) frame 1000))]
    (str (if (or pad-units (> h 0)) (str (pad-left (str h) "0" 2) ":") "")
         (if (or pad-units (> m 0) (> h 0)) (str (pad-left (str m) "0" 2) ":") "")
         (if (or pad-units (> s 0) (> m 0) (> h 0)) (str (pad-left (str s) "0" 2) ":") "")
         (pad-right (str millis) "0" 3))))

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
      (.fillText ctx (str "fm " frame) (/ w 2) (+ (/ h 4) (/ f 4)) w)
      (.fillText ctx (str (frame->timecode frame true)) (/ w 2) (+ (* 3 (/ h 4)) (/ f 4)) w)
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


(defn frame-offset-x [frame scroll-width context]
  (let [frames-per-pixel (/ scroll-width context)]
    (floor (+ 2 (* frames-per-pixel frame)))))

(defn inv-frame-offset-x [x scroll-width context duration]
  (let [frames-per-pixel (/ scroll-width context)]
    (clip 0 (floor (/ x frames-per-pixel)) (dec duration))))

(def skip-levels
  (concat [1 5 10]                                          ;frames per tick
          [30 150 300]                                      ;seconds per tick
          [1800 9000 18000]                                 ;minutes per tick
          [108000]                                          ;hours per tick
          ))

(def tick-proximity-max 16)

(defn max-tick-count [visible-width] (floor (/ visible-width tick-proximity-max)))
(defn current-skip-level [visible-width context]
  (next-or-eq-item (/ context (max-tick-count visible-width)) skip-levels))

(defn shown-tick-frames [visible-width context left-x right-x duration]
  (let [frame-skip (current-skip-level visible-width context)
        left-frame (inv-frame-offset-x left-x visible-width context duration)
        left-frame (- left-frame (mod left-frame frame-skip))
        right-frame (inv-frame-offset-x right-x visible-width context duration)
        right-frame (+ (- right-frame (mod right-frame frame-skip)) frame-skip)
        last-frame (dec duration)
        frames-maybe-missing-end (range left-frame (min right-frame last-frame) frame-skip)
        frames (if (< right-frame last-frame)
                 frames-maybe-missing-end
                 (concat frames-maybe-missing-end [last-frame]))]
    frames))

(defn jump-to-frame! [frame]
  (println "jump to " frame)
  (swap! app-state update-in [:now] (fn [_] (clip 0 frame (:duration @app-state))))
  ;jump scroll-x
  )

(defn abs [a] (.abs js/Math a))

(defonce -context-scroller nil)
(declare start-scrolling-context!)
(declare stop-scrolling-context!)
(defn -context-scroller-fn [_now]
  (let [ctx (get-in @app-state [:timeline :context])
        tgt (get-in @app-state [:timeline :target-context])
        src (get-in @app-state [:timeline :source-context])
        remaining (abs (- tgt ctx))
        ;todo:something smart with _now, dt stuff
        ;ratio (- 1 (/ remaining (abs (- tgt src))))
        vel (/ (- tgt src) 15)]                             ; take approx 15 frames to get there
    (if (<= remaining (abs vel))
      (do
        (swap! app-state update-in [:timeline :context] (fn [_] tgt))
        (stop-scrolling-context!))
      (do
        (swap! app-state update-in [:timeline :context] (fn [_] (+ ctx vel)))
        (start-scrolling-context!)))))

(defn stop-scrolling-context! []
  (when -context-scroller (.cancelAnimationFrame js/window -context-scroller-fn)))
(defn start-scrolling-context! []
  (stop-scrolling-context!)
  (set! -context-scroller (.requestAnimationFrame js/window -context-scroller-fn)))


(defn clip-context [ctx scroll-width duration]
  (clip (max-tick-count scroll-width) ctx duration))

(defn change-context! [ctx]
  (println "change context" ctx)
  (let [target (clip-context ctx (:scroll-width @app-state) (:duration @app-state))]
    (swap! app-state update-in [:timeline]
           (fn [old]
             (merge old {:target-context target
                         :source-context (:context old)})))
    (start-scrolling-context!))
  ;jump scroll-x
  )

; Do this goofy declare/remove/define/add dance to make sure we don't put two
; event handlers on the document.
(declare handle-keyboard!)
(.removeEventListener js/document "keydown" handle-keyboard!)
(defn handle-keyboard! [e]
  (println "K:" (.-keyCode e) "shift:" (.-shiftKey e))
  (let [now (:now @app-state)
        context (:context (:timeline @app-state))
        duration (:duration @app-state)
        ;left-x (get-in @app-state [:timeline :scroll-x])
        scroll-width (:scroll-width @app-state)
        shown-ticks (shown-tick-frames scroll-width
                                       context
                                       0
                                       (frame-offset-x (dec duration) scroll-width context)
                                       duration)
        max-ticks (max-tick-count scroll-width)
        nearest-tick-left (prev-item now shown-ticks)
        nearest-tick-right (next-item now shown-ticks)
        context-step (floor (* duration 0.05))
        frame-skip (current-skip-level scroll-width context)
        nearest-skip-down (prev-item frame-skip skip-levels)
        nearest-skip-up (next-item frame-skip skip-levels)

        shift (.-shiftKey e)]
    (case (.-keyCode e)
      38 (change-context! (if shift                         ; up
                            (* nearest-skip-up max-ticks)
                            (+ context context-step)))
      40 (change-context! (if shift                         ; down
                            (* nearest-skip-down max-ticks)
                            (- context context-step)))
      37 (jump-to-frame! (if shift                          ; left
                           nearest-tick-left
                           (dec now)))
      39 (jump-to-frame! (if shift                          ; right
                           nearest-tick-right
                           (inc now)))
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
                    :onWheel  (fn [e]
                                (let [dx (.-deltaX e)
                                      dy (.-deltaY e)]
                                  (when (and (not= 0 dy) (= (abs dx) 0))
                                    (do
                                      (om/transact! (:timeline data)
                                                    []
                                                    (fn [{ctx :context :as timeline}]
                                                      (let [new-ctx (clip-context (floor (+ ctx dy))
                                                                                  (:scroll-width @app-state)
                                                                                  (:duration @app-state))]
                                                        (assoc timeline
                                                          :context new-ctx
                                                          :source-context new-ctx
                                                          :target-context new-ctx))))))))
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
                                                                   (+ scroll-x w)
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
                      :timeline     {:context  900
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