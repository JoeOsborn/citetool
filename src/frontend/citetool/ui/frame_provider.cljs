(ns ^:figwheel-always citetool.ui.frame-provider
  (:require [cljs.core.async :as async]))

; Provider should be an {:out pub :in chan} map
(defn request-frame [provider frame]
  (let [resp (async/chan)]
    (async/tap (:out provider) resp)
    (async/put! (:in provider) {:frame frame})
    {:channel resp :stand-in {:image "" :frame -1000}}))
