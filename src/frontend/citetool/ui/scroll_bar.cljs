(ns ^:figwheel-always citetool.ui.scroll-bar
  (:require [om.core :as om]
            [cljs.core.async :as async]
            [om.dom :as dom])
  (:require-macros [cljs.core.async.macros :as async-m]))


(defn scroll-thumb-width [data]
  (let [scroll-width (get-in data [:scroll-width])
        total-width (get-in data [:total-width])
        visible-portion (/ scroll-width total-width)]
    (max 32 (* scroll-width visible-portion))))

(defn scroll-thumb-x [data]
  (let [scroll-width (get-in data [:scroll-width])
        total-width (get-in data [:total-width])
        scroll-x (get-in data [:scroll-x])
        thumb-width (scroll-thumb-width data)]
    (min (- scroll-width thumb-width)
         (* scroll-width (/ scroll-x total-width)))))

(defn scroll-bar-bg-on-click! [owner e]
  (let [data (om/get-props owner)
        px (.-pageX e)
        thumb-x (scroll-thumb-x data)
        cb! (get-in data [:callback])
        scroll-x (get-in data [:scroll-x])
        scroll-width (get-in data [:scroll-width])]
    (if (<= px thumb-x)
      (cb! (- scroll-x scroll-width))
      (cb! (+ scroll-x scroll-width)))
    nil))

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
            up-listener (fn [e]
                          (when (= (.-button e) 0)
                            (.preventDefault e)
                            (.stopPropagation e)
                            (async/put! up [(.-pageX e) (.-pageY e)]))
                          nil)
            move-listener (fn [e]
                            (.preventDefault e)
                            (.stopPropagation e)
                            (async/put! move [(.-pageX e) (.-pageY e)])
                            nil)]
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
                                     start-scroll-x (get-in props [:scroll-x])
                                     scroll-width (get-in props [:scroll-width])
                                     total-width (get-in props [:total-width])
                                     cb! (get-in props [:callback])
                                     scaled-to-duration (fn [dx] (* (/ dx scroll-width) total-width))
                                     do-scroll! (fn [x] (cb! (+ start-scroll-x
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
                           :onMouseDown (fn [e]
                                          (when (= (.-button e) 0)
                                            (.preventDefault e)
                                            (.stopPropagation e)
                                            (async/put! down-c [(.-pageX e) (.-pageY e)]))
                                          nil)}))))))

(defn scroll-bar [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div (clj->js {:style   {:position        "fixed"
                                   :bottom          0
                                   :left            0
                                   :width           "100%"
                                   :height          16
                                   :backgroundColor "lightgray"}
                         ; click to jump thumb and change scrollx
                         :onClick (partial scroll-bar-bg-on-click! owner)})
               (om/build scroll-thumb data)))))
