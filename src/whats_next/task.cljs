(ns whats-next.task
  "Show an overview of a task."
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.calendar :refer [calendar-view]]

            [whats-next.state :as state]
            [whats-next.utils :as $]

            [whats-next.shared.task-selection :refer [task-selector]]))

(defn classify-day [day work]
  (when work "worked"))

(defn day-totals [work]
  (map state/total-duration (state/day-groups-contiguous work)))

(def day-count -21)

(defn task-summary-view [work owner]
  (reify
    om/IRenderState
    (render-state [_ _]
      (let [totals (day-totals work)]
        (dom/ul #js {}
                (dom/li nil "Last 7 days: "
                        ($/pretty-duration
                         (reduce + (take 7 totals))))
                (dom/li nil "Last 30 days: "
                        ($/pretty-duration
                         (reduce + (take 30 totals)))))))))

(defn task-view [app owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [task-type end-date]}]
      (dom/div
       #js {:className "task-overview-container"}
       (dom/h3 nil "Task: "
               (dom/div #js {:className "task-select"}
                        (task-selector app
                                       #(om/set-state! owner :task-type %)
                                       task-type)))

       (om/build calendar-view app
                 {:state {:day-count day-count
                          :end-date end-date
                          :transduce-work (state/type-filter task-type)
                          :classify-day classify-day}})

       (om/build task-summary-view (sequence (comp
                                              (state/before (.getTime end-date))
                                              (state/type-filter task-type))
                                             (:work app)))))))
