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
                               task-map timeline-duration]
                        :or {guide-width 1500000
                             render-width 150}}]
        (let [work (if include-gaps (into [] (state/with-gaps) work) work)
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
                          (for [[{ttype :type :as task} ntask] (partition 2 1 (reverse (cons nil work)))
                                :let [d (duration task)
                                      p (/ d total)
                                      w (* render-width p)]]
                            (if (state/gap? task)
                              (dom/span #js {:className "gap"
                                             :style #js {:width w}})

                              (dom/span #js {:className (str "task "
                                                             (when (= ttype (:type ntask))
                                                               "same-as-next"))
                                             :data-taskType ttype
                                             :style #js {:width w}}
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
                                                     (dom/li nil note)))))))))

                   ;; When gaps are included, show the start time of the
                   ;; first task on the timeline.
                   (when include-gaps
                     (dom/div #js {:className "timeline-start"}
                              ($/pretty-time ($/->date (:started (peek work)))))))))))
