(ns whats-next.overview
  (:require [om.core :as om]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state]))

(defn overview-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/ul nil
              ))))
