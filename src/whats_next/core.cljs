(ns ^:figwheel-always whats-next.core
    (:require [cljs.core.async :refer [<! >! chan close!]]

              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]

              [whats-next.csv :as csv]
              [whats-next.pouch :as pouch]
              [whats-next.state :as state]
              [whats-next.utils :as $])
    (:require-macros [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defonce css-react-class
  (-> js/React (aget "addons") (aget "CSSTransitionGroup")))

(defonce css-transition
  (.createFactory js/React css-react-class))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {}))

(defn log-view [app owner]
  (reify
    om/IRender
    (render [_]
      (let [get-type (state/task-map app)
            current (:current-task app)]
        (dom/div nil
                 (dom/table #js {:className "log"}
                            (for [{:keys [type started ended]} (:work app)
                                  :let [t (get-type type)]]
                              (dom/tr nil
                                      (dom/td #js {:className "log-entry"}
                                              (dom/div #js {:className "task-name"}
                                                       type)
                                              (dom/div #js {:className "duration"}
                                                       ($/pretty-duration (- ended started)))
                                              #_(dom/div nil
                                                       ($/pretty-relative-date
                                                        ))
                                              (dom/div #js {:className "symbol"}
                                                       (:symbol t))))))
                 (dom/a #js {:href "#"
                             :onClick (state/goto-handler app :main)}
                        "Back")
                 #_
                 (dom/table #js {:className "navbar"}
                            (dom/tr nil
                                    (dom/td #js {:className "nav-button"}
                                            (dom/a #js {:href "#"
                                                        :onClick (state/goto-handler app :main)}
                                                   "Back")))))))))

(defn quick-buttons [app owner]
  (reify
    om/IRender
    (render [_]
      (if-let [types (:task-types app)]
        (apply dom/div #js {:className "quick-buttons"}
               (for [task-type (:task-types app)]
                 (dom/a #js {:className "button"
                             :href "#"
                             :onClick #(om/transact! app
                                                     (fn [app] (state/start-task app (:name task-type))))} (:symbol task-type))))

        (dom/div #js {:className "quick-buttons empty"}
                 "No Recent Tasks")))))

(defn timer-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:timer-chan ($/interval-chan 1000)
       :duration 0})

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
    (render-state [_ {:keys [duration]}]
      (let [task (:current-task app)
            task-name (:type task)
            task-type (state/get-type app task-name)]
        (dom/div nil
                 (dom/div #js {:className "title-box"}
                          (dom/span #js {:className "symbol"}
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
                                      "✓")))))))

(defn start-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text ""})

    om/IRenderState
    (render-state [_ {:keys [text]}]
      (dom/div nil
               (om/build quick-buttons app)
               (dom/input #js {:className "big"
                               :placeholder "What's Next?"
                               :value text
                               :onChange #(om/set-state! owner :text
                                                         (.. % -target -value))
                               :onKeyDown (fn [e]
                                            (when (= (.-keyCode e) 13)
                                              (om/transact!
                                               app
                                               #(state/start-task % text))))})
               (dom/button #js {:className "start"
                                :type "button"
                                :onClick (fn [e]
                                           (om/transact!
                                            app
                                            #(state/start-task % text)))}
                           "Start")
               (dom/table #js {:className "navbar"}
                          (dom/tr nil
                                  (dom/td #js {:className "nav-button"}
                                          (dom/a #js {:href "#"} "Hi"))
                                  (dom/td #js {:className "nav-button"}
                                          (dom/a #js {:href "#"
                                                      :onClick (state/goto-handler app :log)}
                                                 "Work Log"))))))))

(defn root-view [app-state owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "app-container"}
               (case (:view app-state)
                 :timer (om/build timer-view app-state)
                 :log (om/build log-view app-state)
                 (om/build start-view app-state))))))

(defn main []
  (om/root root-view app-state {:target (.getElementById js/document "app")}))

(defn init []
  (main)

  (add-watch app-state :title-watch (fn [old new _ _]
                                      (set! (.-title js/document)
                                            (if-let [t (:title new)]
                                              (str "What's Next? - " t)
                                              "What's Next?")))))

(defn deinit []
  (remove-watch app-state :title-watch))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

  (println "Reloaded")

  (main))
