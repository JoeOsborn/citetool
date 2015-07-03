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

; max-time determines how unit padding happens.
; if it's <0, units will always be padded out to hours.
; if it's 0, units will never be padded.
; otherwise, units will be padded so that all timecodes
; have the same number of units as max-time.
(defn frame->timecode [frame max-time]
  (let [h (floor (/ frame 108000))
        frame (- frame (* h 108000))
        m (floor (/ frame 1800))
        frame (- frame (* m 1800))
        s (floor (/ frame 30))
        frame (- frame (* s 30))
        ; frame to millisecond = (seconds/frame) * frame * (milliseconds/second)
        millis (floor (* (/ 1 30) frame 1000))

        max-time (if (< max-time 0) Infinity max-time)
        max-h (floor (/ max-time 108000))
        max-time (- max-time (* h 108000))
        max-m (floor (/ max-time 1800))
        max-time (- max-time (* m 1800))
        max-s (floor (/ max-time 30))]
    (str (if (> max-h 0) (str (pad-left (str h) "0" 2) ":") "")
         (if (or (> max-m 0) (> max-h 0)) (str (pad-left (str m) "0" 2) ":") "")
         (if (or (> max-s 0) (> max-m 0) (> max-h 0)) (str (pad-left (str s) "0" 2) ".") "")
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
      (.fillText ctx (str (frame->timecode frame (:duration @app-state))) (/ w 2) (+ (* 3 (/ h 4)) (/ f 4)) w)
      (.toDataURL offscreen-canvas))))

(defn frame-image-provider []
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
            (async/>! responses {:frame frame, :image-data (frame-image-data frame)})
            (recur frame))
          (do
            (println "Unrecognized request " request)
            (recur last-frame)))))
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

(defn clip-scroll [x scroll-width context duration]
  (clip 0 x (+ 16 (- (* duration (/ scroll-width context)) scroll-width))))

(defn clip-context [ctx scroll-width duration]
  (clip (max-tick-count scroll-width) ctx duration))

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
        clipped-new-scroll-x (clip-scroll new-scroll-x scroll-width context duration)]
    (assoc new-timeline
      :scroll-x clipped-new-scroll-x)))

(defn autoscroll-x-playhead [_old-timeline new-timeline]
  (let [duration (:duration @app-state)
        left-buffer 64                                      ;px
        right-buffer 96                                     ;px
        ;is it left of scroll-x+buffer? or: is it right of scroll-x+scroll-width - buffer?
        scroll-width (:scroll-width new-timeline)
        context (:context new-timeline)
        now-x (frame-offset-x (:now new-timeline) scroll-width context)
        new-scroll-x (:scroll-x new-timeline)
        left-edge (+ new-scroll-x left-buffer)
        right-edge (- (+ new-scroll-x scroll-width) right-buffer)
        new-scroll-x (cond
                       (< now-x left-edge) (- now-x left-buffer)
                       (> now-x right-edge) (+ (- now-x scroll-width) right-buffer)
                       true new-scroll-x)
        clipped-new-scroll-x (clip-scroll new-scroll-x scroll-width context duration)]
    (assoc new-timeline
      :scroll-x clipped-new-scroll-x)))

(defn jump-to-frame! [frame autoscroll?]
  (println "jump to" frame)
  (om/transact! (om/root-cursor app-state) [:timeline]
                (fn [old-timeline]
                  (let [new-timeline (assoc old-timeline
                                       :now (clip 0 frame (:duration @app-state)))]
                    (if autoscroll?
                      (autoscroll-x-playhead old-timeline
                                             new-timeline)
                      new-timeline)))))

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
                        (autoscroll-x-context old (assoc old :context tgt) scroll-focus-frame)))
        (stop-scrolling-context!))
      (do
        (om/transact! (om/root-cursor app-state) [:timeline]
                      (fn [old]
                        (autoscroll-x-context old (assoc old :context (+ ctx vel)) scroll-focus-frame)))
        (start-scrolling-context!)))))

(defn stop-scrolling-context! []
  (when -context-scroller (.cancelAnimationFrame js/window -context-scroller-fn)))
(defn start-scrolling-context! []
  (stop-scrolling-context!)
  (set! -context-scroller (.requestAnimationFrame js/window -context-scroller-fn)))

; Do this goofy declare/remove/define/add dance to make sure we don't put two
; event handlers on the document.
(defn change-context! [ctx scroll-focus-frame]
  (println "change context" ctx)
  (let [target (clip-context ctx (:scroll-width (:timeline @app-state)) (:duration @app-state))]
    (set! -scroll-focus-frame scroll-focus-frame)
    (om/transact! (om/root-cursor app-state) [:timeline]
                  (fn [old]
                    (merge old {:target-context target
                                :source-context (:context old)})))
    (start-scrolling-context!)))
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
                           (dec now))
                         true)
      39 (jump-to-frame! (if shift                          ; right
                           nearest-tick-right
                           (inc now))
                         true)
      true)
    (.preventDefault e)))

(.addEventListener js/document "keydown" handle-keyboard!)

(defn scroll-to! [x]
  (let [scroll-width (:scroll-width (:timeline @app-state))
        context (:context (:timeline @app-state))
        duration (:duration @app-state)]
    (om/transact! (om/root-cursor app-state)
                  [:timeline :scroll-x]
                  #(clip-scroll x scroll-width context duration))))

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
    (jump-to-frame! (inv-frame-offset-x mx scroll-width ctx dur) false)))

(defn tick-mark [data _owner {w :width h :height}]
  (reify
    om/IRender
    (render [_]
      (dom/div (clj->js {:style {:border          "1px solid black"
                                 :width           "1px"
                                 :backgroundColor "white"
                                 :height          "96%"
                                 :position        :absolute
                                 :left            (+ 6 (frame-offset-x (:frame data) w (:context data)))
                                 :top             0
                                 :pointer-events  "none"}})))))

(defn tick-mark-label [data _owner {w :width}]
  (reify
    om/IRender
    (render [_]
      (let [frame (:frame data)
            duration (:duration data)
            frame-x (frame-offset-x frame w (:context data))
            label-text (frame->timecode frame duration)
            label-width (* 5 tick-proximity-max)
            font-size 14
            is-last (= frame (dec duration))]
        (dom/p (clj->js {:style (merge
                                  {:position       :absolute
                                   :top            0
                                   :color          "lightgray"
                                   :font-size      (str font-size "px")
                                   :pointer-events "none"}
                                  (if is-last
                                    {:right 8}
                                    {:left (+ frame-x 10)}))})
               label-text)))))

(defn multiples [n]
  (map #(* % n) (range 1 Infinity)))

(defn pixels->frames [px scroll-width context]
  (let [framesperpx (/ context scroll-width)]
    (* px framesperpx)))

(defn frames->pixels [f scroll-width context]
  (let [pxperframe (/ scroll-width context)]
    (* f pxperframe)))

(defn timecode-label-width [label-str font-size]
  (let [len (.-length label-str)]
    (* len font-size)))

(defn findp [pred coll]
  (let [[result-code idx]
        (reduce (fn [[code idx] item]
                  (if (pred item)
                    (reduced [:found idx])
                    [code (inc idx)]))
                [:not-found 0]
                coll)]
    (if (= result-code :found)
      idx
      :not-found)))

(defn playback-controls [data owner]
  (reify
    om/IRender
    (render [_]
      (let [scroll-width (get-in data [:timeline :scroll-width])
            now (get-in data [:timeline :now])
            duration (get-in data [:duration])
            h 28
            btn-width 28
            tc-width (timecode-label-width (frame->timecode duration duration) 8)
            w (+ tc-width (* 3 btn-width) 12)
            btn-style {:backgroundColor "lightGray"
                       :width           btn-width
                       :height          h
                       :fontSize        14
                       :display         :inline-block
                       :textAlign       :center
                       :lineHeight      (str h "px")
                       :borderLeft "1px solid black"
                       :borderRight "1px solid black"}
            tc-style (assoc btn-style
                       :width tc-width
                       :textAlign :center
                       :paddingLeft "5px"
                       :borderRight :none)]
        (dom/div (clj->js {:style {:position        "fixed"
                                   :bottom          "16px"
                                   :left            (- (/ scroll-width 2) (/ w 2))
                                   :width           w
                                   :height          h
                                   :backgroundColor "black"}})
                 (dom/div (clj->js {:style btn-style})
                          "bak")
                 (dom/div (clj->js {:style btn-style})
                          "p/p")
                 (dom/div (clj->js {:style btn-style})
                          "fwd")
                 (dom/div (clj->js {:style tc-style})
                          (frame->timecode now duration))
                 (dom/div (clj->js {:style (assoc btn-style
                                             :position :absolute
                                             :left (- (* 3 btn-width) 1)
                                             :top (/ h 4)
                                             :width (/ btn-width 2)
                                             :height (/ h 2)
                                             :lineHeight (str (/ h 2) "px")
                                             :fontSize 12
                                             :border "1px solid black")})
                          "lnk"))))))

(defn timeline [data owner {h :height y :y}]
  (reify
    om/IDidMount
    (did-mount [_]
      (scroll-to! (.-scrollLeft (om/get-node owner))))
    om/IDidUpdate
    (did-update [_ _ _]
      ;(precache! (get-in (om/get-props owner) [:source]) min max step)
      (set! (.-scrollLeft (om/get-node owner)) (get-in (om/get-props owner) [:timeline :scroll-x])))
    om/IRender
    (render [_]
      (let [context (get-in data [:timeline :context])
            scroll-x (get-in data [:timeline :scroll-x])
            now (get-in data [:timeline :now])
            duration (get-in data [:duration])
            w (get-in data [:timeline :scroll-width])
            frame-resolution (current-skip-level w context)
            max-label-width (timecode-label-width (frame->timecode duration duration) 8)
            timecode-width-in-frames (pixels->frames max-label-width w context)
            tick-label-resolution (first (filter #(>= % timecode-width-in-frames)
                                                 (multiples frame-resolution)))
            ;preview-image-width (pixels->frames (* (/ 4 3) h) ...)
            ;preview-image-resolution (least multiple of frame-resolution >= p-i-w)
            tick-frames (shown-tick-frames w
                                           context
                                           (- scroll-x (/ w 4))
                                           (+ scroll-x w (/ w 2))
                                           duration)
            tick-mark-data (vec (map (fn [f]
                                       (let [base {:frame f :context context}
                                             special (if (= (mod f tick-label-resolution) 0)
                                                       {:label true :duration duration}
                                                       {})]
                                         (merge base special)))
                                     tick-frames))
            final-frame (dec duration)
            has-final-frame (= (last tick-frames) final-frame)
            last-label-safe-frame (- final-frame (* 2 timecode-width-in-frames))
            tick-mark-data (if has-final-frame
                             (map #(cond
                                    (= (:frame %) final-frame) (assoc % :label true :duration duration)
                                    (>= (:frame %) last-label-safe-frame) (dissoc % :label)
                                    true %)
                                  tick-mark-data)
                             tick-mark-data)
            tick-opts {:opts {:width w :height h}}]
        (dom/div
          (clj->js {:style   {:overflow-x "hidden"
                              :overflow-y "none"
                              :position   :absolute
                              :left       0
                              :top        (str y "px")
                              :width      (str w "px")
                              :height     (str h "px")}
                    :onWheel (partial timeline-on-wheel! owner)})
          (dom/div (clj->js {:style   {:backgroundColor "rgb(50,50,200)"
                                       :overflow-y      "none"
                                       :position        "absolute"
                                       :width           (str (+ (* (/ w context) duration) 16) "px")
                                       :height          (str h "px")}
                             :onClick (partial timeline-on-click! owner)})
                   (dom/div (clj->js {:style {:border              "16px solid green"
                                              :border-left-width   "8px"
                                              :border-right-width  "8px"
                                              :border-bottom-width "0px"
                                              :left                8
                                              :width               (str (* (/ w context) duration) "px")
                                              :height              (- h 32)}}))
                   (apply dom/div nil (om/build-all tick-mark tick-mark-data tick-opts))
                   (dom/div (clj->js {:style {:border         "4px solid red"
                                              :width          "4px"
                                              :height         "90%"
                                              :position       :absolute
                                              :pointer-events "none"
                                              :left           (+ 1 (frame-offset-x now w context))
                                              :top            0}}))
                   (apply dom/div nil (om/build-all tick-mark-label (filter #(:label %) tick-mark-data) tick-opts)))
          (om/build playback-controls data)
          (om/build scroll-bar data)
          )))))

(defonce app-state (atom {:source      (frame-image-provider)
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
        (dom/div (clj->js {})
                 (om/build async-image
                           {:now    (get-in data [:timeline :now])
                            :source (:source data)
                            :width  640
                            :height 480}
                           {:opts {:style {:margin-left 80}}})
                 (om/build timeline
                           {:timeline (:timeline data)
                            :edits    (:edits data)
                            :duration (:duration data)
                            :source   (:source data)}
                           {:opts {:height 100 :y 478}})))))
  app-state
  {:target (.getElementById js/document "app")})
