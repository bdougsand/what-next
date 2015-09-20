(ns whats-next.log
  (:require [om.core :as om]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state]
            [whats-next.utils :as $]))

(defn log-view [app owner]
  (reify
    om/IRender
    (render [_]
      (let [get-type (state/task-map app)
            current (:current-task app)]
        (dom/div #js {:className "log-viewer"}
                 (dom/table #js {:className "log"}
                            (for [{:keys [type started ended]} (:work app)
                                  :let [t (get-type type)]]
                              (dom/tr nil
                                      (dom/td #js {:className "log-entry"}
                                              (dom/div #js {:className "task-name"}
                                                       type)
                                              (dom/div #js {:className "duration"}
                                                       ($/pretty-duration (- ended started))
                                                       " — "
                                                       ($/pretty-relative-date
                                                        ended))
                                              (dom/div #js {:className "symbol"}
                                                       (:symbol t)))))))))))
