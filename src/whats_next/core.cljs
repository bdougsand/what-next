(ns whats-next.core
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
              [whats-next.start :refer [start-view]]
              [whats-next.task :refer [task-view]]
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
                    (apply dom/tr nil
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

  (js/console.log "Reloaded")

  (main))
