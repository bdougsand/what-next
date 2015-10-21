(ns whats-next.goals
  "Page for setting daily goals."
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [sablono.core :as html :refer-macros [html]]

            [whats-next.conditions :as cnd]
            [whats-next.state :as state]
            [whats-next.utils :as $]

            [whats-next.shared.task-selection :refer [task-selector]]))


(defn render-goal [goal]
  (html
   (let [{:keys [total conditions]} goal
         {:keys [type time]} conditions]
     [:li.goal
      [:span.duration ($/pretty-duration-med total)]
      " on "
      [:span.tasks (cond (set? type) ($/commas type "or")
                         (keyword? type) (str type)
                         (nil? type) "all tasks")]

      [:span.time
       (cond (keyword? time) (str " " (cnd/name-for-recurring time))
             (vector? time) (str " between " ($/pretty-relative-date
                                              (first time))
                                 " and " ($/pretty-relative-date
                                          (second time)))
             (nil? time) " total ")]])))

(defn goals-view [app owner]
  (reify
      om/IWillMount
      (will-mount [_]
        )

      om/IRenderState
      (render-state [_ _]
        (let [tasks (state/task-map app)]
          (html
           [:div.goals-container
            [:ul.goal-list
             (map render-goal [{:total 3600000 :conditions {:time :daily}}])]
            [:hr]
            [:h3 "Add Goal"]
            [:input {:type "number", :value 25}] " minutes"
            (task-selector app #(om/set-state! owner :task %))
            [:select
             {:onChange (fn [e])}
             [:option {:value "daily"} "Daily"]
             [:option {:value "weekly"} "Weekly"]]])))))
