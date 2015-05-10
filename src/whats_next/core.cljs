(ns ^:figwheel-always whats-next.core
    (:require [cljs.core.async :refer [<! >! chan close!]]

              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]



              ;; Persist the contents of the app atom
              [alandipert.storage-atom :refer [local-storage]]

              [whats-next.csv :as csv]
              [whats-next.state :as state :refer [total-duration]]
              [whats-next.utils :as $])
    (:require-macros [cljs.core.async.macros :refer [go-loop go]])
    (:import [goog.ui.emoji PopupEmojiPicker]))

(enable-console-print!)

(defonce css-react-class
  (-> js/React (aget "addons") (aget "CSSTransitionGroup")))

(defonce css-transition
  (.createFactory js/React css-react-class))

(defonce app-state
  (local-storage (atom {:view :main
                        :view-stack [:main]})
                 :app))

(def view-buttons
  {:main [["Work Log" :log]]
   :timer [["Work Log" :log]]
   :log [["Back" state/go-back]]})

(defn navbar-view [app owner]
  (reify
    om/IRender
    (render [_]
      (let [view (peek (:view-stack app))]
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

(defn log-view [app owner]
  (reify
    om/IRender
    (render [_]
      (let [get-type (state/task-map app)
            current (:current-task app)]
        (dom/div #js {:className "log-viewer"}
                 (dom/table #js {:className "log"}
                            (for [{:keys [type started ended]} (:work app)
                                  :let [t (get-type type)]]
                              (dom/tr nil
                                      (dom/td #js {:className "log-entry"}
                                              (dom/div #js {:className "task-name"}
                                                       type)
                                              (dom/div #js {:className "duration"}
                                                       ($/pretty-duration (- ended started))
                                                       " — "
                                                       ($/pretty-relative-date
                                                        ended))
                                              (dom/div #js {:className "symbol"}
                                                       (:symbol t)))))))))))

(defn timer-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [task (:current-task app)
            today-log (into [] (state/for-day ($/now)) (:work app))
            type-log (into [] (state/type-filter task) today-log)]
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
                          (dom/a #js {:className "symbol"}
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
                                                 (om/transact!
                                                  app
                                                  #(state/add-note-to-current
                                                    % text)))))
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

(defn quick-buttons [app owner]
  (reify
    om/IInitState
    (init-state [_]
      ;; The quick button currently highlighted:
      {:hover-type nil})

    om/IRenderState
    (render-state [_ {:keys [hover-type]}]
      (if-let [types (:task-types app)]
        (dom/div nil
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
                              :onClick #(om/transact! app
                                                      (fn [app] (state/start-task app (:name task-type))))} (:symbol task-type)))))

        (dom/div #js {:className "quick-buttons empty"}
                 "No Recent Tasks")))))

(defn summary-view
  "Constructs a component that summarizes the day's work."
  [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "day-summary"}
               (let [work (filter (state/for-today) (:work app))
                     groups (group-by :type work)
                     tmap (state/task-map app)]
                 (for [[type-name tasks] groups]
                   (let [task-type (tmap type-name)]
                     (dom/div #js {:className "task-summary"}
                              (dom/span #js {:className "symbol"}
                                        (:symbol task-type))
                              (dom/span #js {:className ""}
                                        ($/pretty-duration (total-duration tasks)))))))))))

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
               (om/build summary-view app)))))

(defn root-view [app-state owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "app-container"}
               (case (peek (:view-stack app-state))
                 :timer (om/build timer-view app-state)
                 :log (om/build log-view app-state)
                 (om/build start-view app-state))

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
