(ns whats-next.goals
  "Page for setting daily goals."
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state]
            [whats-next.utils :as $]

            [whats-next.shared.task-selection :refer [task-selector]]))

(defn goals-view [app owner]
  (reify
    om/IRenderState
    (render-state [_ _]
      (dom/div
       #js {:className "goals-container"}
       (let [])))))
