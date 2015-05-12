(ns whats-next.emoji
  (:require [cljs.core.async :refer [<! >! chan close! put!]]

            [goog.events :as events])

  (:import [goog.ui.emoji PopupEmojiPicker]
           [goog.ui.Component EventType]))

(defn get-symbol
  [elt]
  (let [c (chan)
        chooser (PopupEmojiPicker. "/image/none.gif")]
    (events/listen EventType.ACTION
                   (fn [e]
                     (put! c (.getSelectedEmoji chooser))
                     (close! c)))
    (.render chooser)
    (.attach chooser elt)
    c))