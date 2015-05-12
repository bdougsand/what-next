(ns whats-next.calendar
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state]
            [whats-next.utils :as $]))

(def day-count -10)

(defn calendar-view [app owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [task-type end-date]}]
      (dom/div
       #js {:className "calendar-container"}
       (dom/div
        #js {:className "calendar"}
        (dom/div
           #js {:className "col-labels"}
           (for [week-day $/short-day-names]
             (dom/div #js {:className "day-col"} week-day)))
        (let [month (.getMonth end-date)
              start-date ($/start-of-week ($/inc-date end-date day-count))
              work-by-date (state/day-groups
                            (sequence
                             (comp (state/between (.getTime start-date)
                                                  (.getTime end-date))
                                   (state/type-filter task-type))
                             (:work app)))]
          (for [week (partition-all 7 ($/date-range start-date end-date))]
            (dom/div #js {:className "week"}
                     (for [date week
                           :let [work (work-by-date ($/day-components date))]]
                       (dom/div #js {:className (str "day "
                                                     (when (= (.getMonth date) month)
                                                       "this-month ")
                                                     (when work
                                                       "worked"))}
                                (dom/div #js {:className "date"}
                                         (.getDate date))))))))

       (dom/div #js {:className "task-select"}
                (apply dom/select
                       #js {:onChange #(om/set-state! owner :task-type
                                                      (.. % -target -value))}
                       (for [{n :name} (:task-types app)]
                         (dom/option #js {:value n} n))))))))
