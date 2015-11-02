(ns whats-next.timer
  (:require [cljs.core.async :refer [<! close!]]

            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [goog.events :as events]
            [goog.events.KeyCodes :as kc]

            [whats-next.emoji :as emoji]
            [whats-next.state :as state :refer [total-duration]]
            [whats-next.utils :as $]

            [whats-next.api.audio :as audio]
            [whats-next.shared.pomodori :as pom])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn on-keyup [app e]
  (when (= (.. e -target -tagName toLowerCase) "body")
    (condp = (.-keyCode e)
      kc/ESC (om/transact! app state/cancel)
      kc/SPACE (om/transact! app state/complete)
      kc/R (om/transact! app state/restart)
      nil)))

;; Show an
(def alert-interval (* 10 60000))

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
         :duration-today-type (total-duration type-log)
         :listener-key nil

         ;; Bookkeeping, to prevent double-alerts:
         ;; The interval count for the current session:
         :last-duration-alert 0
         ;; The interval count of the last alert for the current task:
         :last-task-duration-alert 0}))

    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :timer-chan)]
        (go-loop []
          (let [duration (- (.valueOf (js/Date.))
                            (get-in (om/get-props owner)
                                    [:current-task :started]))]
            (om/set-state! owner :duration duration)

            ;; Produce an alert if the user has been working for a
            ;; multiple of alert-interval
            (let [i-count (quot duration alert-interval)]
              (when (> i-count (om/get-state owner :last-duration-alert))
                (audio/play-audio "/sounds/coin.mp3")
                (om/set-state! owner :last-duration-alert i-count))))

          (when (<! c)
            (recur))))

      ;; Attach the key listener:
      (om/set-state! owner :listener-key
                     (events/listen js/document "keyup"
                                    (partial on-keyup app))))

    om/IWillUnmount
    (will-unmount [_]
      (close! (om/get-state owner :timer-chan))

      (some->  (om/get-state owner :listener-key) (events/unlistenByKey)))

    om/IRenderState
    (render-state [_ {:keys [duration duration-today duration-today-type
                             last-duration-alert]}]
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
                          (when (pos? last-duration-alert)
                            (pom/pomodori last-duration-alert))
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
                                :placeholder "Add notes"})
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
