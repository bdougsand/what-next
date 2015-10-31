(ns ^:figwheel-always whats-next.core
    (:require [cljs.core.async :refer [<! >! chan close! timeout]]

              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]

              ;; Persist the contents of the app atom
              [alandipert.storage-atom :refer [local-storage]]

              ;; Views:
              [whats-next.chains :as chain]
              [whats-next.conditions :as cnd]
              [whats-next.daily :refer [daily-totals-view]]
              [whats-next.export-work :refer [export-view]]
              [whats-next.task :refer [task-view]]
              [whats-next.timeline :refer [timeline-view]]
              [whats-next.timer :refer [timer-view]]

              [whats-next.csv :as csv]
              [whats-next.emoji :as emeoji]
              [whats-next.goals :refer [goals-view]]
              [whats-next.log :refer [log-view]]
              [whats-next.state :as state :refer [total-duration]]
              [whats-next.utils :as $])
    (:require-macros [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defonce app-state
  (local-storage (atom (state/clean-state))
                 :app))

(defn goto-task-view
  [app]
  (conj (:view-stack app)
        [:task {:task-type (or (:type (:current-task app))
                               (:type (first (:work app))))}]))

(def view-buttons-map
  {:main [["Work Log" :log]
          ["Task Overview" goto-task-view]
          ["Goals" :goals]
          #_["Export" :export]]
   :timer [["Work Log" :log] ["Task Overview" goto-task-view] ["Dailies" :daily]]})

(defn view-buttons [view]
  (view-buttons-map view [["Back" state/go-back]]))

(defn navbar-view [app owner]
  (reify
    om/IRender
    (render [_]
      (let [[view] (peek (:view-stack app))]
        (dom/div nil
                 (dom/table #js {:className "navbar"}
                    (dom/tr nil
                            (for [[label goto] (view-buttons view)]
                              (dom/td #js {:className "nav-button"
                                           :key label}
                                      (dom/a #js {:href "#"
                                                  :onClick (state/goto-handler app goto)}
                                             label)))))

                 (when-let [ct (:current-task app)]
                   (dom/div #js {:className "status-info"}
                            "In Progress: "
                            (:type ct))))))))

(defn quick-buttons [app owner]
  (reify
    om/IInitState
    (init-state [_]
      ;; The quick button currently highlighted:
      {:hover-type nil})

    om/IRenderState
    (render-state [_ {:keys [hover-type]}]
      (if-let [types (:task-types app)]
        (dom/div #js {:className "qb-container"}
                 (dom/div #js {:className (str "quick-hover"
                                               (when-not hover-type
                                                 " empty"))}
                  (:name hover-type
                         "Quick Start"))
         (apply dom/div #js {:className "quick-buttons"}
                (for [task-type (state/recent-types app 8)]
                  (dom/a #js {:className "button"
                              :href "#"
                              :title (str "Start Working on \"" (:name task-type) "\"")
                              :onMouseEnter #(om/set-state! owner :hover-type task-type)
                              :onMouseLeave #(om/set-state! owner :hover-type nil)
                              :onFocus #(om/set-state! owner :hover-type task-type)
                              :onBlur #(om/set-state! owner :hover-type nil)
                              :onClick #(om/transact! app
                                                      (fn [app] (state/start-task app (:name task-type))))} (:symbol task-type)))))

        (dom/div #js {:className "quick-buttons empty"}
                 "No Recent Tasks")))))

(defn task-summary
  [_ owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [expanded? task-type work =]}]
      (dom/div #js {:className "task-summary"
                    :onClick (fn [e]
                               (om/set-state! owner :expanded? (not expanded?))
                               nil)}
               (dom/span #js {:className "symbol"}
                         (:symbol task-type))
               (dom/span #js {:className ""}
                         ($/pretty-duration (total-duration work)))))))

(defn summary-view
  "Constructs a component that summarizes the day's work."
  [app owner]
  (reify
    om/IRender
    (render [_]
      (let [work (into [] (state/for-today) (:work app))
            groups (group-by :type work)
            tmap (state/task-map app)]
        (dom/div #js {:className "day-summary"}
                 (dom/div #js {:className "title"}
                          ($/pretty-date ($/now)))
                 (for [[type-name tasks] groups]
                   (let [task-type (tmap type-name)]
                     (dom/div #js {:className "task-summary"}
                              (dom/span #js {:className "symbol"}
                                        (:symbol task-type))
                              (dom/span #js {:className "amount"}
                                        ($/pretty-duration (total-duration tasks))))))
                 (dom/div #js {:className "all-tasks-summary"}
                          ($/pretty-duration (total-duration work))))))))

(defn start-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text ""
       :days 0})

    om/IWillMount
    (will-mount [_]
      ;; Force the view to refresh
      (go-loop []
        (when-not (om/get-state owner :dismounted)
          (<! (timeout ($/ms-to-day)))
          (om/update-state! owner :days inc)
          (recur))))

    om/IWillUnmount
    (will-unmount [_]
      (om/set-state! owner :dismounted true))

    om/IRenderState
    (render-state [_ {:keys [text]}]
      (dom/div #js {:className "main-container"}
               (om/build quick-buttons app)
               (when-let [canceled (:canceled-task app)]
                 (dom/div #js {:className "canceled-task"}
                          (str "Did you mean to cancel '" (:type canceled) "'?")
                          (dom/button #js {:className ""
                                           :onClick (fn [e]
                                                      (om/transact!
                                                       app
                                                       state/clear-canceled))}
                                      "Yes")
                          (dom/button #js {:onClick #(om/transact!
                                                      app
                                                      state/restore-canceled)}
                                      "No")))
               (dom/input #js {:className "big"
                               :placeholder "What Next?"
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
               (om/build timeline-view
                         (sequence (state/since ($/start-of-day ($/now))) (:work app))
                         {:state {:render-width 320
                                  :task-map (state/task-map app)
                                  :show-labels true}})
               (om/build summary-view app)))))

(defn root-view [app-state owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "app-container"}
               (let [[v p] (peek (:view-stack app-state))]
                 (case v
                   :task (om/build task-view app-state
                                   {:init-state (assoc p :end-date ($/now))})
                   :daily (om/build daily-totals-view
                                    app-state)
                   :goals (om/build goals-view app-state)
                   :export (om/build export-view app-state
                                     {:init-state p})
                   :timer (om/build timer-view app-state
                                    {:init-state p})
                   :log (om/build log-view app-state
                                  {:init-state p})
                   (om/build start-view app-state
                             {:init-state p})))

               (om/build navbar-view app-state)))))

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
