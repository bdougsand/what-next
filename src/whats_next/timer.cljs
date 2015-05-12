(ns whats-next.timer
  (:require [cljs.core.async :refer [<! close!]]

            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.emoji :as emoji]
            [whats-next.state :as state :refer [total-duration]]
            [whats-next.utils :as $])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn timer-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [task (:current-task app)
            today-log (sequence (state/for-day ($/now)) (:work app))
            type-log (sequence (state/type-filter (:type task)) today-log)]
        {:timer-chan ($/interval-chan 1000)
         :duration 0
         :duration-today (total-duration today-log)
         :duration-today-type (total-duration type-log)}))

    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :timer-chan)]
        (go-loop []
          (om/set-state! owner :duration
                         (- (.valueOf (js/Date.))
                            (get-in app [:current-task :started])))

          (when (<! c)
            (recur)))))

    om/IWillUnmount
    (will-unmount [_]
      (close! (om/get-state owner :timer-chan)))

    om/IRenderState
    (render-state [_ {:keys [duration duration-today duration-today-type]}]
      (let [task (:current-task app)
            task-name (:type task)
            task-type (state/get-type app task-name)
            notes (:notes task)]
        (dom/div nil
                 (dom/div #js {:className "title-box"}
                          (dom/a #js {:className "symbol"
                                      :onClick #(emoji/get-symbol (.-target %))}
                                 (:symbol task-type))
                          (dom/span #js {:className "title"}
                                    task-name))
                 (dom/div #js {:className "timer"}
                          ($/pretty-duration duration))
                 (dom/div #js {:className "actions"}
                          (dom/button #js {:className "action cancel"
                                           :onClick #(om/transact! app state/cancel)}
                                      "×")
                          (dom/button #js {:className "action complete"
                                           :onClick #(om/transact! app state/complete)}
                                      "✓"))
                 (dom/div #js {:className "notes"}
                          (dom/textarea
                           #js {:onKeyDown (fn [e]
                                             (when (= (.-keyCode e) 13)
                                               (let [text (.. e -target -value)]
                                                 (when (pos? (count text))
                                                   (om/transact!
                                                    app
                                                    #(state/add-note-to-current
                                                      % text))
                                                   (set! (.. e -target -value) "")
                                                   (.preventDefault e)))))
                                :placeholder "Add note"})
                          (when notes
                            (dom/div #js {:className "show-notes"}
                                     (dom/ul nil
                                             (for [note notes]
                                               (dom/li nil note))))))
                 (dom/div #js {:className "summary"}
                          (str ($/pretty-duration (+ duration duration-today-type))
                               " today, this task; "
                               ($/pretty-duration (+ duration duration-today))
                               " total")))))))
