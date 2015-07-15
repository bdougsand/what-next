(ns whats-next.api.notification
  (:require [cljs.core.async :refer [chan close! put!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn request-permission []
  (let [c (chan)]
    (try
      (.requestPermission js/Notification
                          (fn [perm]
                            (when (= perm "granted")
                              (put! c :granted))
                            (close! c)))

      (catch js/Error _
        (close! c)))))
