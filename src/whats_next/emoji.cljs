(ns whats-next.emoji
  (:require [cljs.core.async :refer [<! >! chan close! put!]]

            [goog.dom :as gdom]
            [goog.dom.DomHelper]
            [goog.events :as events])

  (:import [goog.dom DomHelper]
           [goog.ui.emoji PopupEmojiPicker]
           [goog.ui.Component EventType]))

(defn get-symbol
  [elt]
  (let [c (chan)
        chooser (PopupEmojiPicker. "/image/none.gif")]
    (.render chooser elt)
    (events/listen EventType.ACTION
                   (fn [e]
                     (put! c (.getSelectedEmoji chooser))
                     (close! c)))
    (.render chooser)
    (.attach chooser elt)
    c))
