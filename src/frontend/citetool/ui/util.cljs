(ns citetool.ui.util)

(def active-debugs #{
                     ;:tc-frames
                     })

(defn debug [key & more]
  (when (active-debugs key)
    (apply println more)))

(defn now [] (.now js/Date))

#_(defn multiple-of? [n divisor]
  (= 0 (mod n divisor)))

#_(defn nearest-above [num divisor remainder]
  (next-or-eq-item num (map #(+ remainder %) (multiples divisor))))

#_(defn nearest-below [num divisor remainder]
  (prev-or-eq-item num (map #(+ remainder %) (multiples divisor))))

(defn page-x->element-x [x elt]
  (+ x (.-scrollLeft elt)))

(defn abs [a] (.abs js/Math a))

(defn floor [a] (.floor js/Math a))
(defn ceil [a] (.ceil js/Math a))

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
                [nil 0]
                coll)]
    (if (= result-code :found)
      idx
      nil)))

(defn memberp [pred coll]
  (first (filter pred coll)))

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

(defn frame-offset-x [frame scroll-width context]
  (let [frames-per-pixel (/ scroll-width context)]
    (floor (+ 2 (* frames-per-pixel frame)))))

(defn inv-frame-offset-x [x scroll-width context duration]
  (let [frames-per-pixel (/ scroll-width context)]
    (clip 0 (floor (/ x frames-per-pixel)) (dec duration))))

(defn frame-visible? [f scroll-x scroll-width context]
  (let [fx (frame-offset-x f scroll-width context)]
    (and (>= fx scroll-x) (<= fx (+ scroll-x scroll-width)))))


(defn frame-skipped-frames [frame-skip visible-width context left-x right-x duration]
  (let [left-frame (inv-frame-offset-x left-x visible-width context duration)
        left-frame (- left-frame (mod left-frame frame-skip))
        right-frame (inv-frame-offset-x right-x visible-width context duration)
        right-frame (+ (- right-frame (mod right-frame frame-skip)) frame-skip)
        last-frame (dec duration)
        frames-maybe-missing-end (range left-frame (min right-frame last-frame) frame-skip)
        frames (if (< right-frame last-frame)
                 frames-maybe-missing-end
                 (concat frames-maybe-missing-end [last-frame]))]
    frames))

(defn closer? [left num right]
  (< (abs (- num left)) (abs (- num right))))

(defn closer [left num right]
  (if (closer? left num right)
    left
    right))