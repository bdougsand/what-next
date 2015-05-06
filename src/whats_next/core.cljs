(ns ^:figwheel-always whats-next.core
    (:require [cljs.core.async :refer [<! >! chan]]

              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]

              [whats-next.pouch :as pouch]
              [whats-next.state :as state])
    (:require-macros [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defonce css-react-class
  (-> js/React (aget "addons") (aget "CSSTransitionGroup")))

(defonce css-transition
  (.createFactory js/React css-react-class))

;; define your app data so that it doesn't get over-written on reload

(def task-types (map (fn [x] {:symbol x}) "PABC"))

(def app-state (atom {:task-types task-types
                      :current-task {:started 1430860055935}
                      :work []}))

(defn make-button [{:keys [symbol] :as task-type}]
  (dom/a #js {:className "button"
              :href "#"} symbol))

(defn quick-buttons [app owner]
  (reify
    om/IRender
    (render [_]
      (let [types (:task-types app)]
        (apply dom/div #js {:className "quick-buttons"}
               (map make-button task-types))))))

(defn timer-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:timer-chan (chan)})

    om/IWillMount
    (will-mount [_]
      )

    om/IRender
    (render [_]
      )))

(defn start-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "app-container"}
               (om/build quick-buttons app)
               (dom/input #js {:className "big"
                               :placeholder "What's Next?"})
               (dom/button #js {:className "start"
                                :type "button"}
                           "Start")
               (dom/table #js {:className "navbar"}
                          (dom/tr nil
                                  (dom/td nil
                                          (dom/a #js {:className "nav-button"}
                                                 "Hi"))
                                  (dom/td nil
                                          (dom/a #js {:className "nav-button"}
                                                 "Work Log"))))))))

(defn root-view [app-state owner]
  (reify
    om/IRender
    (render [_]
      (case (:view app-state)
        :timer (om/build timer-view app-state)
        (om/build start-view app-state)))))

(defn main []
  (om/root root-view app-state {:target (.getElementById js/document "app")}))

(defn init []
  (main)

  (add-watch app-state :title-watch (fn [old new _ _]
                                      (set! (.-title js/document) (:title new)))))

(defn deinit []
  (remove-watch app-state :title-watch))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

  (println "Reloaded")

  (main))
