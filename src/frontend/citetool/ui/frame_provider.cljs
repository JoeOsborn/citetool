(ns ^:figwheel-always citetool.ui.frame-provider
  (:require [cljs.core.async :as async]
            [citetool.ui.util :as u]
            [clojure.set :as s])
  (:require-macros [cljs.core.async.macros :as async-m]))

(defn now [] (.now js/Date))

(defn multiple-of? [n divisor]
  (= 0 (mod n divisor)))

(defn nearest-above [num divisor remainder]
  (u/next-or-eq-item num (map #(+ remainder %) (u/multiples divisor))))

(defn nearest-below [num divisor remainder]
  (u/prev-or-eq-item num (map #(+ remainder %) (u/multiples divisor))))

(defn range-make [m n step]
  (into (sorted-set) (range m (inc n) step)))

#_(defn range-make [m n step]
  (if (not= (mod m step) (mod n step))
    [m (nearest-above n step (mod n step)) step]
    [m n step]))

(defn range-make-single [f] (range-make f f 1))

#_(defn range-make-single [m] (range-make m m 1))

(def range-empty? empty?)

#_(defn range-empty? [[m n step]]
  (or (<= step 0)
      (< n m)))

(defn range- [r1 r2]
  ;(println "subtract" r1 "-" r2 "=" (s/difference r1 r2))
  (s/difference r1 r2))
#_(defn range- [[m1 n1 step1 :as r1] [m2 n2 step2]]
  (if (or (< n2 m1)                                         ; not overlapping
          (> m2 n1)
          (not (multiple-of? step1 step2))                  ; not comparable -- step1 not a multiple of step2
          (not= (mod m1 step2) (mod m2 step2)))             ; not comparable -- r1 out of phase with r2
    r1
    (let [[m2 n2 _step1] [(nearest-above m2 step1 (mod m1 step1)) (nearest-below n2 step1 (mod m1 step1)) step1]
          empty [0 0 0]]
      (cond
        ; 1 inside of 2 or 1 = 2 --> 0
        (and (<= m2 m1) (>= n2 n1)) empty
        ; 2 inside of 1 --> 1 since we can't split the range easily.
        ; _todo: in the future return two ranges [[m1 m2 step] , [n2 n1 step]], admit compound ranges
        (and (<= m1 m2) (>= n1 n2)) r1
        ; 2's right >= 1's left --> 2's right, 1's right; r2 overlaps from the left
        (>= n2 m1) [n2 n1 step1]
        ; 2's left <= 1's right --> 1's left, 2's left; r2 overlaps from the right
        (<= m2 n1) [m1 m2 step1]
        ; otherwise --> just return 1. should not be reached.
        true r1))))

(defn range->min-max-steps [full-r]
  ;  (println "reduce" full-r)
  (reduce (fn [rs f]
            (if (empty? rs)
              ;start with a temp range
              [[f f 1]]
              (let [[rm rn rstep] (last rs)
                    rs' (butlast rs)]
                (if (= rm rn)
                  ;turn temp into real range, maybe with non-unit step
                  (conj rs' [rm f (- f rm)])
                  (if (= (+ rn rstep) f)
                    ;update right limit
                    (conj rs' [rm f rstep])
                    ;new temp range
                    (conj rs [f f 1]))))))
          [] full-r))

#_(defn range->min-max-steps [r] r)

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

(defn cache-get-exact [cache f]
  (let [idx (sorted-index-of cache f)
        s (get cache idx)]
    (if (= (:frame s) f)
      s
      nil)))

(defn cache-update-birthday [cache f date]
  (let [idx (sorted-index-of cache f)
        frame (get cache idx)
        frame-f (:frame frame)]
    (if (= frame-f f)
      (assoc-in cache [idx :birthday] date)
      ; if not present, leave cache alone
      cache)))

(defn trim-cache [cache kill-date]
  ;todo: also only filter ones without active precaches
  (filterv #(>= (:birthday %) kill-date) cache))

;todo: probably better to go through pc's range and find cache entries.
; or at least only go from first-found-entry-index to last-found-entry-index
(defn cache-trim-precache [cache pc]
  (let [now (now)
        ;todo: associate images with pc
        [cache r] (reduce (fn [[cache r1] ci]
                            (if (empty? r1)
                              (reduced [cache r1])
                              (let [r2 (range-make-single (:frame (get cache ci)))
                                    new-r1 (range- r1 r2)]
                                (if (not= r1 new-r1)
                                  [(assoc-in cache [ci :birthday] now) new-r1]
                                  [cache new-r1]))))
                          [cache (:effective-range pc)]
                          (range 0 (count cache)))]
    [cache (assoc pc :effective-range r)]))

(defn actives-trim-precache [providers pc]
  (let [actives (filter #(not (nil? %)) (map second providers))
        r (reduce (fn [r1 r2]
                    (if (empty? r1)
                      (reduced r1)
                      (range- r1 r2)))
                  (:effective-range pc)
                  (map :range actives))]
    (assoc pc :effective-range r)))

(defn queue-drop-with-id [queue pc-id]
  (filterv #(not= (:precache-id %) pc-id) queue))

(def queue-push conj)

#_(def queue-split split-at)

#_(defn queue-trim-precache [queue pc index]
  (let [[queue _ignore] (queue-split queue index)
        r (reduce (fn [r1 r2]
                    (if (empty? r1)
                      (reduced r1)
                      (range- r1 r2)))
                  (:effective-range pc)
                  (map :range queue))]
    (assoc pc :effective-range r)))

(defn maybe-advance-queue! [cache queue providers]
  (if-let [pc (first queue)]
    (if-let [free-src-i (u/findp #(= (second %) nil) providers)]
      (let [[cache pc] (cache-trim-precache cache pc)
            pc (actives-trim-precache providers pc)
            r (:effective-range pc)
            new-queue (vec (rest queue))]
        (if (empty? r)
          (do
            ;todo: unassociate pc with images in cache
            (maybe-advance-queue! cache new-queue providers))
          (let [[provider _] (get providers free-src-i)
                new-providers (assoc-in providers [free-src-i 1] (assoc pc :effective-range r))]
            (async/put! (:in provider) {:ranges (range->min-max-steps r)})
            (maybe-advance-queue! cache new-queue new-providers))))
      [cache queue providers])
    [cache queue providers]))

(defn- split-interval- [m n split acc]
  (if (> n (+ m split))
    (split-interval- (+ m split) n split (conj acc [m (+ m split)]))
    (conj acc [m n])))

(defn split-interval [m n split]
  (split-interval- m n split []))

(defn make-frame-source [providers split]
  ; pull dispatching stuff out of tc-frame-provider and into here. returns :out mult :in chan map.
  ; the actual provider will also return an :out :in map but the out won't be multiplexed.
  (let [pouts (mapv :out providers)
        sin (async/chan)
        sout (async/chan)
        channels (conj pouts sin)]
    (async-m/go-loop
      ;todo: use some avl tree or sorted vec or sorted JS array or something for speed.
      [cache (vector)
       precache-id 0
       queue (vector)
       providers (mapv (fn [p] [p nil]) providers)]
      (let [cache cache providers providers precache-id precache-id queue queue] ;silence IntelliJ warnings
        (let [[val port] (async/alts! channels)]
          (if (= sin port)
            (let [msg val
                  {mtype :type} msg]
              (println "got control message" msg)
              (case mtype
                :request-frame (let [f (:frame msg)
                                     {:keys [image-data frame]} (get-best-standin cache f)
                                     outc (:standin-channel msg)]
                                 (async/put! outc {:stand-in {:image-data image-data :frame frame}})
                                 (if (= f frame)
                                   (do
                                     #_(println "skip request for" f)
                                     (recur (cache-update-birthday cache f (now)) precache-id queue providers))
                                   (do                      ;send standin and precache with set ID and increment pcid
                                     (async/put! sin {:type                :precache-frames
                                                      :range               [f f 1]
                                                      :precache-id-channel outc})
                                     (recur cache precache-id queue providers))))
                :precache-frames (let [[m n step] (:range msg)
                                       intervals (split-interval m n split)
                                       ids (range precache-id (+ precache-id (count intervals)))
                                       pcs (map (fn [[m n] id]
                                                  {:range           [m n step]
                                                   :effective-range (range-make m n step)
                                                   :precache-id     id})
                                                intervals
                                                ids)
                                       ;_ (println "got pcs" pcs)
                                       new-queue (reduce queue-push queue pcs)
                                       [new-cache pcs] (reduce (fn [[cache pcs] pc]
                                                                 (let [[cache pc] (cache-trim-precache cache pc)]
                                                                   (if (empty? (:effective-range pc))
                                                                     [cache pcs]
                                                                     [cache (conj pcs pc)])))
                                                               [cache []]
                                                               pcs)
                                       ;_ (println "second pcs" pcs)
                                       ; todo: remove this line once cache-trim-precache also associates pc with images
                                       pcs (map #(assoc % :effective-range (range-make m n step)) pcs)
                                       ;_ (println "final pcs" pcs)
                                       ;_ (println "Q:" new-queue)
                                       ; todo: add calls to queue-trim and active-trim once pcs are associated with images
                                       ]
                                   ;bounce along out channel all covered cache images
                                   ;todo: do this much smarter!
                                   (doseq [f (range-make m n step)]
                                     (when-let [frame (cache-get-exact new-cache f)]
                                       ; (println "rebounce" f)
                                       (async/put! sout frame)))
                                   (if (empty? pcs)
                                     (do
                                       (when-let [outc {:precache-id-channel msg}]
                                         (async/put! outc {:precache-ids []}))
                                       (recur new-cache precache-id new-queue providers))
                                     (do
                                       (when-let [outc (:precache-id-channel msg)]
                                         (async/put! outc {:precache-ids (map :precache-id pcs)}))
                                       (let [[new-cache new-queue new-providers] (maybe-advance-queue! new-cache
                                                                                                       new-queue
                                                                                                       providers)]
                                         (recur new-cache
                                                (inc (last ids))
                                                new-queue
                                                new-providers)))))
                :cancel-precache (let [pc-id (:precache-id msg)]
                                   ;todo: remove pc-id from images in cache
                                   ;todo: re-trim pcs of lower priority/higher idx than pc
                                   (recur cache precache-id (queue-drop-with-id queue pc-id) providers))))
            ;must be a provider
            (let [frame val pout port
                  now (now)
                  prov-i (u/findp #(= pout %) pouts)
                  [prov src-pc] (get providers prov-i)
                  range (range- (:effective-range src-pc) (range-make-single (:frame frame)))
                  new-pc (assoc src-pc :effective-range range)
                  new-providers providers
                  new-queue queue
                  kill-date (- now 10000)                   ; cache hard time limit
                  frame (assoc frame :birthday now)
                  new-cache (sorted-insert cache frame)
                  new-cache (if (> (count new-cache) 100)   ; cache soft size limit
                              (trim-cache new-cache kill-date)
                              new-cache)]
              #_(println "B broadcast" frame)
              (async/put! sout frame)
              (if (range-empty? range)
                (let [[new-cache new-queue new-providers] (maybe-advance-queue! new-cache
                                                                                new-queue
                                                                                (assoc new-providers prov-i [prov nil]))]
                  (recur new-cache precache-id new-queue new-providers))
                (recur new-cache
                       precache-id
                       queue
                       (assoc-in providers [prov-i 1] new-pc))))))))
    {:out (async/mult sout) :in sin}))

; Source should be an {:out mult :in chan} map
(defn request-frame [source frame cb]
  (let [resp (async/chan)
        standin-resp (async/chan)]
    (async/tap (:out source) resp)
    (async/put! (:in source) {:type :request-frame :frame frame :standin-channel standin-resp})
    (async/take!
      standin-resp
      (fn [{standin :stand-in}]
        (if (= (:frame standin) frame)
          (do
            (async/close! standin-resp)
            (cb {:channel resp :stand-in standin :precache-id nil}))
          (async/take!
            standin-resp
            (fn [{pc-ids :precache-ids}]
              (async/close! standin-resp)
              (cb {:channel resp :stand-in standin :precache-id (first pc-ids)}))))))))

(defn precache [source m n step]
  (let [resp (async/chan)]
    (async/put! (:in source) {:type :precache-frames :range [m n step] :precache-id-channel resp})
    resp))

(defn cancel-precache [source pc-id]
  (when-not (nil? pc-id)
    (async/put! (:in source) {:type :cancel-precache :precache-id pc-id})))