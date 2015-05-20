(ns whats-next.timeline
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state :refer [duration total-duration]]
            [whats-next.utils :as $]))


(defn timeline-view
  "Displays task work in a timeline view.

  State properties:

  - timeline- duration: The total time represented by the timeline. The
  calculated total duration of the given work will be used if this is
  not provided.

  - guide-width: Show guides representing intervals of the specified
  number of milliseconds.

  - include-gaps: If true, also render the gaps between tasks.

  - render-width: The width, in pixels, of the timeline

  - show-labels: If true, show label text in the task blocks."
  [work owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [guide-width include-gaps
                             render-width show-labels
                             timeline-duration]
                      :or {guide-width 1500000
                           render-width 150}}]
      (let [total (or timeline-duration (total-duration work))]
        (dom/div #js {:className "timeline-container"}
                 (when guide-width
                   (let [w (* (/ guide-width total) render-width)]
                     (for [i (range (inc (/ total guide-width)))]
                       (dom/div
                        #js {:className "timeline-guide"
                             :style #js {:left (* i w)
                                         :width w}}))))
                 (for [task (reverse work)
                       :let [d (duration task)
                             p (/ d total)
                             w (* render-width p)]]
                   (dom/span #js {:className "task"
                                  :style #js {:width w}}
                             (when show-labels
                               (:type task))
                             (dom/div
                              #js {:className "timeline-task-info"}
                              (:type task) "\n"
                              ($/pretty-duration (duration task))))))))))
