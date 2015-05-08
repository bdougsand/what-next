(ns whats-next.state
  "Functions for manipulating the application state."
  (:require [om.core :as om]

            [whats-next.utils :as $]))

(defn get-type [app n]
  ($/find-pred #(= n (:name %)) (:task-types app)))

(defn start-task [{:keys [task-types] :as app} type-name]
  (assoc app
    :task-types (if-let [t (get-type app type-name)]
                  task-types
                  (cons {:name type-name
                         :symbol (first type-name)}
                        task-types))
    :current-task {:type type-name
                   :started (.valueOf (js/Date.))}
    :view :timer))

(defn complete
  "Mark the current task as complete."
  [app]
  (let [task (:current-task app)
        type-name (:type task)
        task-type (get-type app type-name)]
    (assoc app
      :current-task nil
      :view nil
      :work (cons (assoc task
                    :ended (.valueOf (js/Date.)))
                  (:work app)))))

(defn cancel
  "Cancel the current task."
  [app]
  (assoc app
    :current-task nil
    :view nil))

(def task-map
  ($/memo-last
   (fn [app]
     (into {} (map (fn [type] [(:name type) type])) (:task-types app)))))


(defn goto-handler
  "Returns an event handler that changes the "
  [cursor view]
  (fn [e]
    (om/update! cursor :view view)))


;; Manipulating tasks
(defn duration [task]
  (- (:ended task) (:started task)))

(defn total-duration [tasks]
  (reduce + (map duration tasks)))
