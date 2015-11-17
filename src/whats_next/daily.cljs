(ns whats-next.daily
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]

            [whats-next.state :as state]
            [whats-next.utils :as $])
  (:import [goog.i18n DateTimeSymbols]))

(defn daily-totals-view [app owner]
  (reify
      om/IInitState
      (init-state [_]
        {:start ($/end-of-day ($/now))
         ;; Number of days to show, counting backward from start.
         :days 10
         ;; What counts as a good day?
         :good (* 250 60000)})

      om/IRenderState
      (render-state [_ {:keys [start days good]}]
        (html
         (let [totals (take days
                            (state/daily-totals (:work app)))
               c (count totals)]
           [:div.daily-container
            [:div.dailies
             (map (fn [[[y m d] ms]]
                    [:div.daily
                     (when (>= ms good)
                       [:div.badge "☺"])
                     [:div.day (let [weekday (->> (js/Date. y (dec m) d)
                                                  (.getDay)
                                                  (aget (.-SHORTWEEKDAYS DateTimeSymbols)))]
                                 (str weekday " " m "/" d "/" y))]
                     ($/pretty-duration-med ms)])
                  totals)]
            (let [total (reduce + (map second totals))]
              [:div.summary
               [:div "Total: " ($/pretty-duration total)]
               [:div (str c "-day average: ")
                ($/pretty-duration
                 (/ total c))]])])))))

#_
(defn daily-totals-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:start ($/end-of-day ($/now))
       ;; Number of days to show, counting backward from start.
       :days 10
       ;; What counts as a good day?
       :good (* 250 60000)})

    om/IRenderState
    (render-state [_ {:keys [start days good]}]
      (let [totals (take days
                         (state/daily-totals (:work app)))
            c (count totals)]
        (dom/div
         #js {:className "daily-container"}
         (apply dom/div
                #js {:className "dailies"}
                (map (fn [[[y m d] ms]]
                       (dom/div #js {:className "daily"}
                                (when (>= ms good)
                                  (dom/div #js {:className "badge"}
                                           "☺"))
                                (dom/div #js {:className "day"}
                                         (str m "/" d "/" y))
                                ($/pretty-duration-med ms)))
                     totals))
         (let [total (reduce + (map second totals))]
           (dom/div #js {:className "summary"}
                    (dom/div nil
                             "Total: " ($/pretty-duration total))
                    (dom/div nil
                             (str c "-day average: ")
                             ($/pretty-duration
                              (/ total c))))))))))
