(ns whats-next.goals
  "Page for setting daily goals."
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]

            [whats-next.conditions :as cnd]
            [whats-next.shared.task-selection :refer [task-selector]]
            [whats-next.state :as state]
            [whats-next.utils :as $]))

(defn- make-goal [owner]
  (let [{:keys [task goaltype time]} (om/get-state owner)]
    {:conditions (merge (case goaltype
                             "today" (cnd/day-condition)
                             "tomorrow" (cnd/day-condition ($/inc-date ($/now)))
                             {:time (keyword goaltype)})
                        {:type task})
     :total (* time 60000)}))

(defn- add-goal [app owner]
  (let [goal (make-goal owner)]
    (when (cnd/valid-goal? goal)
      (om/transact! app (fn [app]
                          (assoc app
                                 :active-goals (conj (:active-goals app)
                                                     goal))))
      goal)))

(defn- remove-goal [app goal]
  (om/transact! app (fn [app]
                      (assoc app
                             :active-goals (vec (remove (partial = goal) (:active-goals app)))))))

(defn- on-change [owner]
  (fn [e]
    (om/set-state! owner (keyword (.. e -target -name))
                   (.. e -target -value))))

(defn set-multi [owner & [kv-pairs]]
  (doseq [[k v] (partition 2 kv-pairs)]
    (om/set-state! owner k v)))

(defn unset-multi! [owner & ks]
  (doseq [k ks] (om/set-state! owner k nil)))

(defn render-goal [app goal]
  (html
   (let [work (:work app)
         {:keys [total conditions]} goal
         {:keys [type time]} conditions]
     [:li.goal
      [:span.duration ($/pretty-duration-med total)]
      " on "
      [:span.tasks (cond (set? type) ($/commas type "or")
                         (string? type) (prn-str type)
                         (nil? type) "all tasks")]

      [:span.time
       (cond (keyword? time) (str " " (cnd/name-for-recurring time))
             (vector? time) (str ($/pretty-relative-date (first time))
                                 " through "
                                 ($/pretty-relative-date (second time)))
             (nil? time) " total ")]
      #_
      (let [progress (cnd/goal-progress goal work)]
        [:span.progress
         (js/Math.floor (* 100 progress)) "%"])

      [:a.delete {:onClick #(remove-goal app goal)
                  :href "#"} "x"]])))

(defn goals-view [app owner]
  (reify
      om/IInitState
      (init-state [_]
        {:time 25
         :goaltype "today"
         :task (state/last-type app)})

      om/IRenderState
      (render-state [_ {:keys [time task goaltype]}]
        (let [tasks (state/task-map app)]
          (html
           [:div.goals-container
            [:ul.goal-list
             (if-let [active (seq (:active-goals app))]
               (map (partial render-goal app) active)

               [:span.empty "No active goals"])]
            [:button {:onClick (fn [e]
                                 (om/transact! app cnd/clear-active-goals)
                                 ($/cancel e))}
             "Clear"]
            [:hr]
            [:h3 "Add Goal"]
            [:form {:onSubmit (fn [e]
                                (add-goal app owner)
                                ($/cancel e))}
             [:input {:type "number"
                      :min "1"
                      :value (or time 25)
                      :name "time"
                      :onChange (on-change owner)}]
             " minutes"
             (task-selector app #(om/set-state! owner :task %) task)
             [:select
              {:onChange (on-change owner)
               :name "goaltype"
               :value goaltype}
              [:option {:value "today"} "Today"]
              [:option {:value "tomorrow"} "Tomorrow"]
              [:option {:value "daily"} "Every Day"]
              [:option {:value "weekly"} "Every Week"]
              [:options {:value "monthly"} "Every Month"]]
             [:br]
             [:button "Create"]]])))))
