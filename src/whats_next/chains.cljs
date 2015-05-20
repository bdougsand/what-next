(ns whats-next.chains
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.state :as state]
            [whats-next.utils :as $]))

(defn count-day-chain
  ([work ref-date]
   (count (take-while true?
                      (map =
                           (sequence (comp
                                      (state/before ($/->stamp ref-date))
                                      (map #($/day-components (js/Date. (:started %))))
                                      (distinct))
                                     work)
                           (map #($/day-components ($/dec-date ref-date %)) (range))))))
  ([work]
   (count-day-chain work ($/now)))
  ([work ref-date task-type]
   (count-day-chain
    (sequence (state/type-filter task-type) work)
    ref-date)))

(defn chain-tasks
  [work ref-date]
  (let [tasks-chain (state/daily-tasks (sequence (state/before ref-date) work))
        types (first tasks-chain)]
    (into {} (map (fn [task-type]
                    (let [] [task-type (take-while #(contains? % task-type) tasks-chain)])))
          types)))

(defn chains-view
  "Show "
  [app owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [to-date]}]
      (let [chains (chain-tasks (:work app) to-date)
            c (count chains)]
        (dom/div #js {:className (str "chains-container"
                                      (when (= 0 c) " empty"))}
                 (dom/h3 nil (str c " chain" (when-not (= c 1) "s") ":"))
                 (dom/ul nil
                         (map (fn [[task-type ]]))))))))
