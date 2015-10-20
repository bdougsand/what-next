(ns whats-next.goals
  "Page for setting daily goals."
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [sablono.core :as html :refer-macros [html]]

            [whats-next.conditions :as cnd]
            [whats-next.state :as state]
            [whats-next.utils :as $]

            [whats-next.shared.task-selection :refer [task-selector]]))

(defn render-condition [[cond-type cond-val]]
  (html
   [:div.condition
    (condp = cond-type
      :time (cond (keyword? cond-val)
                  ["between " ]
                  (str "between")))]))

(defn render-goal [goal]
  (html
   ))

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
           [:li.goal "Hi"]]
          [:hr]
          [:h3 "Add Goal"]
          [:input {:type "number", :value 25}] " minutes"
          (task-selector app #(om/set-state! owner :task %))
          [:select
           {:onChange (fn [e])}
           [:option {:value "daily"} "Daily"]
           [:option {:value "weekly"} "Weekly"]
           ]])))))
