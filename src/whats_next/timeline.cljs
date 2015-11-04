(ns whats-next.timeline
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state :refer [duration total-duration]]
            [whats-next.utils :as $]))

(defn timeline-view
  "Displays task work in a timeline view.

  State properties:

  - xf: Transducer for work

  - timeline- duration: The total time represented by the timeline. The
  calculated total duration of the given work will be used if this is
  not provided.

  - guide-width: Show guides representing intervals of the specified
  number of milliseconds.

  - include-gaps: If true, also render the gaps between tasks.

  - render-width: The width, in pixels, of the timeline

  - show-labels: If true, show label text in the task blocks."
  [app owner]
  (reify
      om/IRenderState
      (render-state [_ {:keys [guide-width include-gaps
                               render-width show-labels
                               task-map timeline-duration
                               xf]
                        :or {guide-width 1500000
                             render-width 150
                             xf (state/since ($/start-of-day ($/now)))}}]
        (let [work (sequence xf (:work app))
              total (or timeline-duration (total-duration work))]
          (dom/div #js {:className "timeline-container"}
                   (apply dom/div
                          (when guide-width
                            (let [w (* (/ guide-width total) render-width)]
                              (for [i (range (/ total guide-width))]
                                (dom/div
                                 #js {:className "timeline-guide"
                                      :style #js {:left (* i w)
                                                  :width w}})))))
                   (apply dom/div
                          #js {}
                          (for [{ttype :type :as task} (reverse work)
                                :let [d (duration task)
                                      p (/ d total)]]
                            (dom/span #js {:className "task"
                                           :data-tasktype ttype
                                           :style #js {:width (* render-width p)}}
                                      (when show-labels
                                          (dom/strong nil
                                                      (or (get-in task-map [ttype :symbol])
                                                          (:type task))))
                                      (dom/div
                                         #js {:className "timeline-task-info"}
                                         (dom/strong nil ttype)
                                         (dom/br nil)
                                         ($/pretty-time ($/->date (:started task)))
                                         "–"
                                         ($/pretty-time ($/->date (:ended task)))
                                         (dom/br nil)
                                         "("
                                         ($/pretty-duration-med (duration task))
                                         ")"
                                         (when-let [notes (:notes task)]
                                           (dom/ul nil
                                                   (for [note notes]
                                                     (dom/li nil note))))))))

                   ;; When gaps are included, show the start time of the
                   ;; first task on the timeline.
                   (when include-gaps
                     (dom/div #js {:className "timeline-start"}
                              ($/pretty-time ($/->date (:started (peek work)))))))))))
