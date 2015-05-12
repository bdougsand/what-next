(ns whats-next.export-work
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.csv :as csv]))

(defn make-csv [work]
  (csv/csv
   (cons ["Task Name" "Start" "End"]
         (map (juxt :type (comp str :started) (comp str :ended)) work))))

(defn export-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/textarea #js {:className "export"}
                    (make-csv (:work app))))))
