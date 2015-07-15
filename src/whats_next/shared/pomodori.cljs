(ns whats-next.shared.pomodori
  (:require [om.dom :as dom]))

(defn pomodori [n]
  (apply dom/div #js {:className "pomodori"}
         (map
          #(dom/div #js {:className "pomodoro"})
          (range n))))
