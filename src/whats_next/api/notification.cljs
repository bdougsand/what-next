(ns whats-next.api.notification
  (:require [chan put!])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn request-permission []
  (let [c (chan)]
    (try
      (.requestPermission js/Notification
                          (fn [perm]
                            (when (= perm "granted")
                              (put! c :granted))
                            (close! c)))

      (catch Exception _
        (close! c)))))

(defn )
