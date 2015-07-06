(ns ^:figwheel-always citetool.ui.frame-provider
  (:require [cljs.core.async :as async]
            [citetool.ui.util :as u])
  (:require-macros [cljs.core.async.macros :as async-m]))

(defn now [] (.now js/Date))

(defn sorted-index-of- [cache f left right d]
  (let [len (- right left)]
    (cond
      (>= d 10) (count cache)
      (= len 0) left
      true (let [mid (+ left (u/floor (/ len 2)))
                 mid-f (:frame (get cache mid))]
             (cond
               (= f mid-f) mid
               (< f mid-f) (sorted-index-of- cache f left mid (inc d))
               (> f mid-f) (sorted-index-of- cache f (inc mid) right (inc d))
               true (do (println "impossible cond" f mid mid-f) (throw "bluh")))))))

(defn sorted-index-of [cache f]
  (let [len (count cache)]
    (sorted-index-of- cache f 0 len 0)))

(defn sorted-insert [cache frame]
  (let [insert-idx (sorted-index-of cache (:frame frame))]
    (if (or (>= insert-idx (count cache))
            (= (:frame frame) (:frame (get cache insert-idx))))
      (assoc cache insert-idx frame)
      (let [[l r] (split-at insert-idx cache)]
        (vec (concat l [frame] r))))))

(defn get-best-standin [cache f]
  (let [idx (sorted-index-of cache f)]
    #_(println "index of" f "in" cache "=" idx)
    (cond
      (empty? cache) {:image-data nil :frame -1000}
      (>= idx (count cache)) (last cache)
      (= idx 0) (first cache)
      true (let [l (get cache (dec idx))
                 r (get cache idx)]
             #_(println "closer l f r" (:frame l) f (:frame r) (u/closer (:frame l) f (:frame r)))
             (if (u/closer? (:frame l) f (:frame r))
               l
               r)))))

(defn trim-cache [cache kill-date]
  ;todo: also only filter ones without active precaches
  (println "old size" (count cache))
  (let [new-cache (filterv #(>= (:birthday %) kill-date) cache)]
    (println "new size" (count new-cache))
    new-cache))

(defn make-frame-source [provider]
  ; pull dispatching stuff out of tc-frame-provider and into here. returns :out mult :in chan map.
  ; the actual provider will also return an :out :in map but the out won't be multiplexed.
  (let [{pout :out pin :in} provider
        sout (async/chan)
        sin (async/chan)]
    (async-m/go-loop
      ;todo: use some avl tree or sorted vec or sorted JS array or something for speed.
      [cache (vector)
       precache-id 0]
      (async-m/alt!
        pout ([frame]
               ; got frame from image provider, put it into cache etc etc
               (let [now (now)
                     kill-date (- now 10000)                ; cache hard time limit 10s
                     frame (assoc frame :birthday now)
                     new-cache (sorted-insert cache frame)
                     new-cache (if (> (count new-cache) 100)  ; cache soft size limit 10
                                 (trim-cache new-cache kill-date)
                                 new-cache)]
                 (async/put! sout frame)
                 (recur new-cache precache-id)))
        sin ([{mtype :type :as msg}]
              (case mtype
                :request-frame (let [f (:frame msg)
                                     {:keys [image-data frame]} (get-best-standin cache f)]
                                 (when-let [outc (:standin-channel msg)]
                                   (async/put! outc {:image-data image-data :frame frame}))
                                 (async/put! sin {:type :precache-frames :range [f (inc f) 1]})
                                 (recur cache precache-id))
                :precache-frames (let [[m n step] (:range msg)]
                                   (when-let [outc (:precache-id-channel msg)]
                                     (async/put! outc {:precache-id precache-id}))
                                   ;todo: something actually good here
                                   (doseq [f (range m n step)]
                                     (async/put! pin {:frame f}))
                                   (recur cache (inc precache-id)))
                :cancel-precache (recur cache precache-id)))))
    {:out (async/mult sout) :in sin}))

; Source should be an {:out mult :in chan} map
(defn request-frame [source frame cb]
  (let [resp (async/chan)
        standin-resp (async/chan)]
    (async/tap (:out source) resp)
    (async/put! (:in source) {:type :request-frame :frame frame :standin-channel standin-resp})
    (async/take! standin-resp
                 (fn [standin]
                   (println "got standin" (:frame standin))
                   (async/close! standin-resp)
                   (cb {:channel resp :stand-in standin})))))
