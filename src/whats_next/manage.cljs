(ns whats-next.manage
  (:require [om.core :as om]
            [om.dom :as dom]

            [whats-next.shared.task-selection :refer [task-selector]]))

(defn manager-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {})

    om/IRenderState
    (render-state [_ {:keys [delete-task]}]
      (dom/div #js {:className "manager-container"}
               (dom/h3 nil "Rename Task")

               (dom/h3 nil "Delete Task")
               (task-selector app #(om/set-state! owner :delete-task %)
                              delete-task)))))
