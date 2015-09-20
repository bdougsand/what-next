(ns whats-next.goals
  "Page for setting daily goals."
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.conditions :as cnd]
            [whats-next.state :as state]
            [whats-next.utils :as $]

            [whats-next.shared.task-selection :refer [task-selector]]))

(defn goals-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      )

    om/IRenderState
    (render-state [_ _]
      (let [tasks (state/task-map app)]
        (dom/div
         #js {:className "goals-container"}
         ;; Show the existing goals:
         (apply dom/ul #js {:className "goal-list"}
                (map (fn [goal]
                       (dom/li #js {:className "goal"}
                               (:task goal)))
                     (cnd/active-conditions app)))
         (let []))))))
