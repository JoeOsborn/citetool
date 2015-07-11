(ns ^:figwheel-always citetool.ui.core
  (:require [figwheel.client :include-macros true]
            [cljs.core.async :as async]
            [clojure.browser.dom]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [citetool.ui.util :as u]
            [citetool.ui.frame-provider :as fp]
            [citetool.ui.tc-frame-provider :as fip]
            [citetool.ui.async-image :as async-image]
            [citetool.ui.scroll-bar :as scroll-bar]))

(enable-console-print!)

(declare on-js-reload)
(declare app-state)

(figwheel.client/watch-and-reload
  :websocket-url "ws://localhost:3449/figwheel-ws"
  :jsload-callback on-js-reload)

(defn on-js-reload []
  (om/transact! (om/root-cursor app-state) [:__figwheel_counter] inc))

(def test-duration 2000)
(defonce app-state (atom {:source      (fp/make-frame-source [(fip/frame-image-provider test-duration)
                                                              (fip/frame-image-provider test-duration)
                                                              (fip/frame-image-provider test-duration)
                                                              (fip/frame-image-provider test-duration)]
                                                             1000)
                          :metadata    {}
                          :timeline    {:context        test-duration
                                        :source-context test-duration
                                        :target-context test-duration
                                        :scroll-x       0
                                        :now            0
                                        :scroll-width   800}
                          :annotations []
                          :edits       []
                          :duration    test-duration}))

(def tick-proximity-max 16)
(defn max-tick-count [visible-width] (u/floor (/ visible-width tick-proximity-max)))

(def skip-levels
  (concat [1 5 10]                                          ;frames per tick
          [30 150 300]                                      ;seconds per tick
          [1800 9000 18000]                                 ;minutes per tick
          [108000]                                          ;hours per tick
          ))

(defn current-skip-level [visible-width context]
  (u/next-or-eq-item (/ context (max-tick-count visible-width)) skip-levels))

(defn clip-scroll-x [x scroll-width context duration]
  (u/clip 0 x (+ 16 (- (* duration (/ scroll-width context)) scroll-width))))

(defn clip-context [ctx scroll-width duration]
  (u/clip (max-tick-count scroll-width) ctx duration))

(defn autoscroll-x-context [old-timeline new-timeline focus-frame]
  (let [duration (:duration @app-state)
        ; using old-timeline:
        scroll-x (:scroll-x old-timeline)
        scroll-width (:scroll-width old-timeline)
        context (:context old-timeline)
        focus-x (u/frame-offset-x focus-frame scroll-width context)
        ; find the offset of that x value from the left hand side of the visible area
        offset (- focus-x scroll-x)
        ; using new-timeline:
        scroll-width (:scroll-width new-timeline)
        context (:context new-timeline)
        ; find the new x value of that frame
        new-focus-x (u/frame-offset-x focus-frame scroll-width context)
        ; scroll to put that new x value at the same offset from the left hand side as before
        new-scroll-x (- new-focus-x offset)
        clipped-new-scroll-x (clip-scroll-x new-scroll-x scroll-width context duration)]
    (assoc new-timeline
      :scroll-x clipped-new-scroll-x)))

(defn autoscroll-x-playhead [_old-timeline new-timeline]
  (let [duration (:duration @app-state)
        left-buffer 64                                      ;px
        right-buffer 96                                     ;px
        ;is it left of scroll-x+buffer? or: is it right of scroll-x+scroll-width - buffer?
        scroll-width (:scroll-width new-timeline)
        context (:context new-timeline)
        now-x (u/frame-offset-x (:now new-timeline) scroll-width context)
        new-scroll-x (:scroll-x new-timeline)
        left-edge (+ new-scroll-x left-buffer)
        right-edge (- (+ new-scroll-x scroll-width) right-buffer)
        new-scroll-x (cond
                       (< now-x left-edge) (- now-x left-buffer)
                       (> now-x right-edge) (+ (- now-x scroll-width) right-buffer)
                       true new-scroll-x)
        clipped-new-scroll-x (clip-scroll-x new-scroll-x scroll-width context duration)]
    (assoc new-timeline
      :scroll-x clipped-new-scroll-x)))

(defn jump-to-frame! [frame autoscroll?]
  (println "jump to" frame)
  (om/transact! (om/root-cursor app-state) [:timeline]
                (fn [old-timeline]
                  (let [new-timeline (assoc old-timeline
                                       :now (u/clip 0 frame (dec (:duration @app-state))))]
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
        remaining (u/abs (- tgt ctx))
        ;todo:something smart with _now, dt stuff
        ;ratio (- 1 (/ remaining (abs (- tgt src))))
        vel (/ (- tgt src) context-animation-duration)]     ; take approx 15 frames to get there
    (if (<= remaining (u/abs vel))
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
        skip (current-skip-level scroll-width context)
        shown-ticks (u/frame-skipped-frames skip
                                            scroll-width
                                            context
                                            0
                                            (u/frame-offset-x (dec duration) scroll-width context)
                                            duration)
        max-ticks (max-tick-count scroll-width)
        nearest-tick-left (u/prev-item now shown-ticks)
        nearest-tick-right (u/next-item now shown-ticks)
        context-step (u/floor (* duration 0.05))
        frame-skip (current-skip-level scroll-width context)
        nearest-skip-down (u/prev-item frame-skip skip-levels)
        nearest-skip-up (u/next-item frame-skip skip-levels)
        focused-frame (if (u/frame-visible? now scroll-x scroll-width context)
                        now
                        (u/inv-frame-offset-x (+ scroll-x (/ scroll-width 2))
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
                  #(clip-scroll-x x scroll-width context duration))))

(defn timeline-on-wheel! [owner e]
  (let [dx (.-deltaX e)
        dy (.-deltaY e)
        data (om/get-props owner)
        scroll-x (:scroll-x (:timeline data))
        scroll-width (:scroll-width (:timeline data))
        context (:context (:timeline data))
        duration (:duration data)
        mouse-frame (u/inv-frame-offset-x (+ scroll-x (/ scroll-width 2)) scroll-width context duration)]
    (cond
      (and (not= 0 dy) (< (u/abs dx) 2))
      (do
        (om/transact! (:timeline data)
                      []
                      (fn [{ctx :context :as timeline}]
                        (let [new-ctx (clip-context (u/floor (+ ctx dy))
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
        mx (u/page-x->element-x (.-pageX e) scroll-box)
        mx (- mx 8)
        scroll-width (:scroll-width (:timeline data))
        ctx (:context (:timeline data))
        dur (:duration data)]
    (jump-to-frame! (u/inv-frame-offset-x mx scroll-width ctx dur) false)))

(defn tick-mark [data _owner {w :width}]
  (reify
    om/IRender
    (render [_]
      (dom/div (clj->js {:style {:border          "1px solid black"
                                 :width           "1px"
                                 :backgroundColor "white"
                                 :height          "96%"
                                 :position        :absolute
                                 :left            (+ 6 (u/frame-offset-x (:frame data) w (:context data)))
                                 :top             0
                                 :pointerEvents   "none"}})))))

(defn tick-mark-label [data _owner {w :width}]
  (reify
    om/IRender
    (render [_]
      (let [frame (:frame data)
            duration (:duration data)
            frame-x (u/frame-offset-x frame w (:context data))
            label-text (u/frame->timecode frame duration)
            font-size 14
            is-last (= frame (dec duration))]
        (dom/p (clj->js {:style (merge
                                  {:position      :absolute
                                   :top           0
                                   :color         "lightgray"
                                   :fontSize      (str font-size "px")
                                   :pointerEvents "none"}
                                  (if is-last
                                    {:right 8}
                                    {:left (+ frame-x 10)}))})
               label-text)))))

(defn preview-frame [data _owner {w :width h :height}]
  (reify
    om/IRender
    (render [_]
      (let [frame (:frame data)
            duration (:duration data)
            pw (:preview-width data)
            frame-x (u/frame-offset-x frame w (:context data))
            frame-x (+ 6 frame-x)
            skip (:skip data)
            is-last (= frame (dec duration))]
        (when-not skip
          (println "f" frame "ox" frame-x)
          (om/build async-image/async-image
                    {:now    frame
                     :source (:source data)
                     :width  pw
                     :height h
                     :attrs  {:style (merge
                                       {:position      :absolute
                                        :top           16
                                        :pointerEvents "none"}
                                       (if is-last
                                         {:right 8}
                                         {:left frame-x}))}}))))))

(defn playback-controls [data _owner]
  (reify
    om/IRender
    (render [_]
      (let [scroll-width (get-in data [:timeline :scroll-width])
            now (get-in data [:timeline :now])
            duration (get-in data [:duration])
            h 28
            btn-width 28
            tc-width (u/timecode-label-width (u/frame->timecode duration duration) 8)
            w (+ tc-width (* 3 btn-width) 12)
            btn-style {:backgroundColor "lightGray"
                       :width           btn-width
                       :height          h
                       :fontSize        14
                       :display         :inline-block
                       :textAlign       :center
                       :lineHeight      (str h "px")
                       :borderLeft      "1px solid black"
                       :borderRight     "1px solid black"}
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
                          (u/frame->timecode now duration))
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
            max-label-width (u/timecode-label-width (u/frame->timecode duration duration) 8)
            timecode-width-in-frames (u/pixels->frames max-label-width w context)
            tick-label-resolution (first (filter #(>= % timecode-width-in-frames)
                                                 (u/multiples frame-resolution)))
            tick-frames (u/frame-skipped-frames frame-resolution
                                                w
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
            preview-height (- h 24)
            preview-width (* (/ 4 3) preview-height)
            preview-width-in-frames (u/floor (u/pixels->frames preview-width w context))
            preview-frames (u/frame-skipped-frames (u/floor preview-width-in-frames)
                                                   w
                                                   context
                                                   (- scroll-x (/ w 4))
                                                   (+ scroll-x w (/ w 2))
                                                   duration)
            preview-frame-data (vec (map (fn [f]
                                           {:frame         f :context context :duration duration
                                            :preview-width preview-width
                                            :source        (get-in data [:source])})
                                         preview-frames))
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
            last-preview-safe-frame (- final-frame preview-width-in-frames)
            preview-frame-data (if has-final-frame
                                 (map #(cond
                                        (= (:frame %) final-frame) %
                                        (>= (:frame %) last-preview-safe-frame) (assoc % :skip true)
                                        true %)
                                      preview-frame-data)
                                 preview-frame-data)
            tick-opts {:opts {:width w :height h} :key :frame}
            preview-opts {:opts {:width w :height preview-height}}]
        (dom/div
          (clj->js {:style   {:overflowX "hidden"
                              :overflowY "none"
                              :position  :absolute
                              :left      0
                              :top       (str y "px")
                              :width     (str w "px")
                              :height    (str h "px")}
                    :onWheel (partial timeline-on-wheel! owner)})
          (dom/div (clj->js {:style   {:backgroundColor "rgb(50,50,200)"
                                       :overflowY       "none"
                                       :position        "absolute"
                                       :width           (str (+ (* (/ w context) duration) 16) "px")
                                       :height          (str h "px")}
                             :onClick (partial timeline-on-click! owner)})
                   (dom/div (clj->js {:style {:border            "16px solid green"
                                              :borderLeftWidth   "8px"
                                              :borderRightWidth  "8px"
                                              :borderBottomWidth "0px"
                                              :left              8
                                              :width             (str (* (/ w context) duration) "px")
                                              :height            (- h 32)}}))
                   (apply dom/div nil (om/build-all preview-frame preview-frame-data preview-opts))
                   (apply dom/div nil (om/build-all tick-mark tick-mark-data tick-opts))
                   (dom/div (clj->js {:style {:border        "4px solid red"
                                              :width         "4px"
                                              :height        "90%"
                                              :position      :absolute
                                              :pointerEvents "none"
                                              :left          (+ 1 (u/frame-offset-x now w context))
                                              :top           0}}))
                   (apply dom/div nil (om/build-all tick-mark-label (filter #(:label %) tick-mark-data) tick-opts)))
          (om/build playback-controls data)
          (om/build scroll-bar/scroll-bar
                    {:total-width  (* duration (/ w context))
                     :scroll-width w
                     :scroll-x     scroll-x
                     :callback     scroll-to!}))))))

(om/root
  (fn [data _owner]
    (reify om/IRender
      (render [_]
        (dom/div (clj->js {})
                 (om/build async-image/async-image
                           {:now    (get-in data [:timeline :now])
                            :source (:source data)
                            :width  640
                            :height 480
                            :attrs  {:style {:marginLeft 80}}})
                 (om/build timeline
                           {:timeline (:timeline data)
                            :edits    (:edits data)
                            :duration (:duration data)
                            :source   (:source data)}
                           {:opts {:height 100 :y 478}})))))
  app-state
  {:target (.getElementById js/document "app")})
