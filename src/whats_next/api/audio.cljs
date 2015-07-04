(ns whats-next.api.audio
  (:require [cljs.core.async :refer [chan <! put!]]

            [goog.dom :as dom]
            [goog.events :as events]
            [goog.style :as style])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def loaded-sounds
  (atom {}))

(defn load-and-play-audio [src]
  (dom/appendChild
   (.-body js/document)
   (doto (dom/createElement "audio")
     (.setAttribute "src" src)
     (.setAttribute "autoplay" "true")
     (style/setStyle "display" "none")
     (.load))))

(defn play-audio [src]
  (if-let [audio (get @loaded-sounds src)]
    (do
      (set! (.-currentTime audio) 0)
      (.play audio))
    (load-and-play-audio src)))
