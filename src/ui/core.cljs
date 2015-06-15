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
  (om/transact! (om/root-cursor app-state) [:__figwheel_counter] inc))

(defonce offscreen-canvas (clojure.browser.dom/html->dom "<canvas/>"))

(defn page-x->element-x [x elt]
  (+ x (.-scrollLeft elt)))

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
         (if (or pad-units (> s 0) (> m 0) (> h 0)) (str (pad-left (str s) "0" 2) ".") "")
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

(defn frame-visible? [f scroll-x scroll-width context]
  (let [fx (frame-offset-x f scroll-width context)]
    (and (>= fx scroll-x) (<= fx (+ scroll-x scroll-width)))))

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


(defn abs [a] (.abs js/Math a))

(defn autoscroll-x-context [old-timeline new-timeline focus-frame]
  (let [duration (:duration @app-state)
        ; using old-timeline:
        scroll-x (:scroll-x old-timeline)
        scroll-width (:scroll-width old-timeline)
        context (:context old-timeline)
        focus-x (frame-offset-x focus-frame scroll-width context)
        ; find the offset of that x value from the left hand side of the visible area
        offset (- focus-x scroll-x)
        ; using new-timeline:
        scroll-width (:scroll-width new-timeline)
        context (:context new-timeline)
        ; find the new x value of that frame
        new-focus-x (frame-offset-x focus-frame scroll-width context)
        ; scroll to put that new x value at the same offset from the left hand side as before
        new-scroll-x (- new-focus-x offset)
        clipped-new-scroll-x (clip 0
                                   new-scroll-x
                                   (- (* (/ scroll-width context) duration) scroll-width))]
    (println "Old offset" offset "New offset" (- new-focus-x clipped-new-scroll-x))
    (println "Old x" focus-x "new x" new-focus-x "old scroll" (:scroll-x old-timeline) "new scroll" clipped-new-scroll-x)
    (assoc new-timeline
      :scroll-x clipped-new-scroll-x)))

(defn autoscroll-x-playhead [old-timeline new-timeline]
  (let [; see above but use a different policy
        new-scroll-x (:scroll-x new-timeline)]
    (assoc new-timeline :scroll-x new-scroll-x)))

(defn jump-to-frame! [frame]
  (println "jump to" frame)
  (om/transact! (om/root-cursor app-state) [:timeline]
                (fn [old-timeline]
                  (autoscroll-x-playhead old-timeline
                                         (assoc old-timeline
                                           :now (clip 0 frame (:duration @app-state)))))))

(defonce -context-scroller nil)
(declare start-scrolling-context!)
(declare stop-scrolling-context!)
(defonce -scroll-focus-frame 0)
(def context-animation-duration 10)
(defn -context-scroller-fn [_now]
  (let [ctx (get-in @app-state [:timeline :context])
        tgt (get-in @app-state [:timeline :target-context])
        src (get-in @app-state [:timeline :source-context])
        scroll-focus-frame -scroll-focus-frame
        remaining (abs (- tgt ctx))
        ;todo:something smart with _now, dt stuff
        ;ratio (- 1 (/ remaining (abs (- tgt src))))
        vel (/ (- tgt src) context-animation-duration)]     ; take approx 15 frames to get there
    (if (<= remaining (abs vel))
      (do
        (om/transact! (om/root-cursor app-state) [:timeline]
                      (fn [old]
                        (println "swapping to" (:scroll-x (autoscroll-x-context old (assoc old :context tgt) scroll-focus-frame)))
                        (autoscroll-x-context old (assoc old :context tgt) scroll-focus-frame)))
        (stop-scrolling-context!))
      (do
        (om/transact! (om/root-cursor app-state) [:timeline]
                      (fn [old]
                        (println "swapping to" (:scroll-x (autoscroll-x-context old (assoc old :context (+ ctx vel)) scroll-focus-frame)))
                        (autoscroll-x-context old (assoc old :context (+ ctx vel)) scroll-focus-frame)))
        (start-scrolling-context!)))))

(defn stop-scrolling-context! []
  (when -context-scroller (.cancelAnimationFrame js/window -context-scroller-fn)))
(defn start-scrolling-context! []
  (stop-scrolling-context!)
  (set! -context-scroller (.requestAnimationFrame js/window -context-scroller-fn)))


(defn clip-context [ctx scroll-width duration]
  (clip (max-tick-count scroll-width) ctx duration))

(defn change-context! [ctx scroll-focus-frame]
  (println "change context" ctx)
  (let [target (clip-context ctx (:scroll-width (:timeline @app-state)) (:duration @app-state))]
    (set! -scroll-focus-frame scroll-focus-frame)
    (om/transact! (om/root-cursor app-state) [:timeline]
                  (fn [old]
                    (merge old {:target-context target
                                :source-context (:context old)})))
    (start-scrolling-context!)))

; Do this goofy declare/remove/define/add dance to make sure we don't put two
; event handlers on the document.
(declare handle-keyboard!)
(.removeEventListener js/document "keydown" handle-keyboard!)
(defn handle-keyboard! [e]
  (println "K:" (.-keyCode e) "shift:" (.-shiftKey e))
  (let [now (:now (:timeline @app-state))
        context (:context (:timeline @app-state))
        duration (:duration @app-state)
        scroll-x (get-in @app-state [:timeline :scroll-x])
        scroll-width (:scroll-width (:timeline @app-state))
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
        focused-frame (if (frame-visible? now scroll-x scroll-width context)
                        now
                        (inv-frame-offset-x (+ scroll-x (/ scroll-width 2))
                                            scroll-width
                                            context
                                            duration))
        shift (.-shiftKey e)]
    (case (.-keyCode e)
      38 (change-context! (if shift                         ; up
                            (* nearest-skip-up max-ticks)
                            (+ context context-step))
                          focused-frame)
      40 (change-context! (if shift                         ; down
                            (* nearest-skip-down max-ticks)
                            (- context context-step))
                          focused-frame)
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

(defn scroll-to! [x]
  (let [scroll-width (:scroll-width (:timeline @app-state))
        context (:context (:timeline @app-state))
        duration (:duration @app-state)]
    (om/transact! (om/root-cursor app-state)
                  [:timeline :scroll-x]
                  #(clip 0 x (+ 16 (- (* duration (/ scroll-width context)) scroll-width))))))

(defn scroll-thumb-width [data]
  (let [duration (get-in data [:duration])
        scroll-width (get-in data [:timeline :scroll-width])
        context (get-in data [:timeline :context])
        total-width (* duration (/ scroll-width context))
        visible-portion (/ scroll-width total-width)]
    (max 32 (* scroll-width visible-portion))))

(defn scroll-thumb-x [data]
  (let [duration (get-in data [:duration])
        scroll-width (get-in data [:timeline :scroll-width])
        context (get-in data [:timeline :context])
        total-width (* duration (/ scroll-width context))
        scroll-x (get-in data [:timeline :scroll-x])
        thumb-width (scroll-thumb-width data)]
    (min (- scroll-width thumb-width)
         (* scroll-width (/ scroll-x total-width)))))

(defn scroll-bar-bg-on-click! [owner e]
  (let [px (.-pageX e)
        data (om/get-props owner)
        thumb-x (scroll-thumb-x data)
        tl-cur (:timeline (om/get-props owner))]
    (if (<= px thumb-x)
      (scroll-to! (- (get-in tl-cur [:scroll-x])
                     (get-in tl-cur [:scroll-width])))
      (scroll-to! (+ (get-in tl-cur [:scroll-x])
                     (get-in tl-cur [:scroll-width]))))))

(defn add-scroll-listeners! [move up]
  (.addEventListener js/window "mousemove" move)
  (.addEventListener js/window "mouseup" up))

(defn remove-scroll-listeners! [move up]
  (.removeEventListener js/window "mousemove" move)
  (.removeEventListener js/window "mouseup" up))

(defn scroll-thumb [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:down    (async/chan)
       :up      (async/chan)
       :move    (async/chan)
       :unmount (async/chan)})
    om/IDidMount
    (did-mount [_]
      ; drag to move thumb and change scrollx
      (let [{down :down up :up move :move unmount :unmount} (om/get-state owner)
            up-listener #(do
                          (when (= (.-button %) 0)
                            (.preventDefault %)
                            (.stopPropagation %)
                            (async/put! up [(.-pageX %) (.-pageY %)]))
                          true)
            move-listener #(do
                            (.preventDefault %)
                            (.stopPropagation %)
                            (async/put! move [(.-pageX %) (.-pageY %)])
                            true)]
        ; little state machine.
        ; A. looks for unmount (and closes chans) or for down.
        ; B. if down, adds move and up event listeners to js/window and loops waiting for:
        ;   B1a. unmount (close chans, eval to :unmount)
        ;   B1b. move (change scroll-x and recur) or
        ;   B1c. up (change scroll-x, eval to :up)
        ;   B2. then it removes those listeners and
        ;   B3. if the result was not :unmount it recurs back to (A).
        (async-m/go-loop []
                         (async-m/alt!
                           unmount :unmount
                           down
                           ([[start-x _y]]
                             (do
                               (add-scroll-listeners! move-listener up-listener)
                               (let [props (om/get-props owner)
                                     start-scroll-x (get-in props [:timeline :scroll-x])
                                     scroll-width (get-in props [:timeline :scroll-width])
                                     duration (get-in props [:duration])
                                     context (get-in props [:timeline :context])
                                     total-width (* duration (/ scroll-width context))
                                     scaled-to-duration (fn [dx]
                                                          (* (/ dx scroll-width) total-width))
                                     do-scroll! (fn [x] (scroll-to! (+ start-scroll-x
                                                                       (scaled-to-duration (- x start-x)))))
                                     loop-result
                                     (loop []
                                       (async-m/alt!
                                         unmount :unmount
                                         move ([[x _y]]
                                                (do-scroll! x)
                                                (recur))
                                         up ([[x _y]]
                                              (do-scroll! x)
                                              :up)))]
                                 (remove-scroll-listeners! move-listener up-listener)
                                 (when-not (= loop-result :unmount)
                                   (recur)))))))))
    om/IWillUnmount
    (will-unmount [_]
      (let [{down :down up :up move :move unmount :unmount} (om/get-state owner)]
        (async/put! unmount :unmount)
        (doseq [c [down up move unmount]] (async/close! c))))
    om/IRender
    (render [_]
      (let [thumb-width (scroll-thumb-width data)
            thumb-x (scroll-thumb-x data)
            down-c (:down (om/get-state owner))]
        (dom/div (clj->js {:style       {:position        "fixed"
                                         :width           thumb-width
                                         :height          16
                                         :left            thumb-x
                                         :bottom          0
                                         :backgroundColor "darkgray"
                                         :borderRadius    16}
                           :onMouseDown #(do                ; must return true to avoid a React warning
                                          (when (= (.-button %) 0)
                                            (.preventDefault %)
                                            (.stopPropagation %)
                                            (async/put! down-c [(.-pageX %) (.-pageY %)]))
                                          true)}))))))

(defn scroll-bar [data owner]
  (reify
    om/IRender
    (render [_]
      (let [thumb-width (scroll-thumb-width data)
            thumb-x (scroll-thumb-x data)]
        (dom/div (clj->js {:style   {:position        "fixed"
                                     :bottom          0
                                     :left            0
                                     :width           "100%"
                                     :height          16
                                     :backgroundColor "lightgray"}
                           ; click to jump thumb and change scrollx
                           :onClick (partial scroll-bar-bg-on-click! owner)})
                 (om/build scroll-thumb data))))))

(defn timeline-on-wheel! [owner e]
  (let [dx (.-deltaX e)
        dy (.-deltaY e)
        data (om/get-props owner)
        scroll-x (:scroll-x (:timeline data))
        scroll-width (:scroll-width (:timeline data))
        context (:context (:timeline data))
        duration (:duration data)
        mouse-frame (inv-frame-offset-x (+ scroll-x (/ scroll-width 2)) scroll-width context duration)]
    (cond
      (and (not= 0 dy) (< (abs dx) 2))
      (do
        (om/transact! (:timeline data)
                      []
                      (fn [{ctx :context :as timeline}]
                        (let [new-ctx (clip-context (floor (+ ctx dy))
                                                    scroll-width
                                                    duration)]
                          (autoscroll-x-context timeline
                                                (assoc timeline
                                                  :context new-ctx
                                                  :source-context new-ctx
                                                  :target-context new-ctx)
                                                mouse-frame)))))
      true (scroll-to! (+ scroll-x dx)))))

(defn timeline-on-click! [owner e]
  (let [scroll-box (om/get-node owner)
        data (om/get-props owner)
        mx (page-x->element-x (.-pageX e) scroll-box)
        mx (- mx 8)
        scroll-width (:scroll-width (:timeline data))
        ctx (:context (:timeline data))
        dur (:duration data)]
    (jump-to-frame! (inv-frame-offset-x mx scroll-width ctx dur))))

(defn timeline [data owner {h :height y :y}]
  (reify
    om/IDidMount
    (did-mount [_]
      (om/transact! (:timeline data)
                    [:scroll-x]
                    (fn [_] (.-scrollLeft (om/get-node owner)))))
    om/IDidUpdate
    (did-update [_ _ _]
      (println "did update" (get-in data [:timeline :scroll-x]))
      (set! (.-scrollLeft (om/get-node owner)) (get-in data [:timeline :scroll-x])))
    om/IRender
    (render [_]
      (let [context (get-in data [:timeline :context])
            scroll-x (get-in data [:timeline :scroll-x])
            now (get-in data [:timeline :now])
            duration (get-in data [:duration])
            w (get-in data [:timeline :scroll-width])]
        (println "am rendering" scroll-x)
        (dom/div
          (clj->js {:style   {:overflow-x "hidden"
                              :position   :absolute
                              :left       0
                              :top        (str y "px")
                              :width      (str w "px")
                              :height     (str h "px")}
                    :onWheel (partial timeline-on-wheel! owner)})
          (dom/div (clj->js {:style   {:backgroundColor     "rgb(50,50,200)"
                                       :border              "16px solid green"
                                       :border-left-width   "8px"
                                       :border-right-width  "8px"
                                       :border-bottom-width "0px"
                                       :width               (str (* (/ w context) duration) "px")
                                       :height              (str (- h 31) "px")}
                             :onClick (partial timeline-on-click! owner)}))
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
                                     :top            0}}))
          (om/build scroll-bar data)
          )))))

(def app-state (atom {:source      (frame-image-provider)
                      :metadata    {}
                      :timeline    {:context        900
                                    :source-context 900
                                    :target-context 900
                                    :scroll-x       0
                                    :now            0
                                    :scroll-width   800}
                      :annotations []
                      :edits       []
                      :duration    2000}))

(om/root
  (fn [data _owner]
    (reify om/IRender
      (render [_]
        (dom/div {}
                 (om/build async-image {:now (get-in data [:timeline :now]) :source (:source data)})
                 (om/build timeline
                           {:timeline (:timeline data)
                            :edits    (:edits data)
                            :duration (:duration data)}
                           {:opts {:height 100 :y 478}})))))
  app-state
  {:target (.getElementById js/document "app")})
