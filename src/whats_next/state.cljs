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
                  (conj task-types
                        {:name type-name
                         :symbol (first type-name)}))
    :current-task {:type type-name
                   :started (.valueOf (js/Date.))}
    :view-stack (conj (:view-stack app) :timer)))

(defn complete
  "Mark the current task as complete."
  [app]
  (let [task (:current-task app)
        type-name (:type task)
        task-type (get-type app type-name)]
    (assoc app
      :current-task nil
      :view-stack (pop (:view-stack app))
      :work (conj (:work app)
                  (assoc task
                    :ended (.valueOf (js/Date.)))))))

(defn cancel
  "Cancel the current task."
  [app]
  (assoc app
    :current-task nil
    :view-stack (pop (:view-stack app))))


;; Navigation
(defn goto-handler
  "Returns an event handler will change the current view to the
  specified view."
  [cursor view]
  (cond
   (keyword? view)
   (fn [e]
     (prn "Going to" view)
     (om/transact! cursor :view-stack #(conj % view))
     (.preventDefault e))

   (ifn? view)
   (fn [e]
     (prn "Going back")
     (om/transact! cursor :view-stack view)
     (.preventDefault e))))

(defn go-back [view-stack]
  (pop view-stack))

;; Manipulating tasks
(defn duration [task]
  (- (:ended task) (:started task)))

(defn total-duration [tasks]
  (reduce + (map duration tasks)))

(def task-map
  ($/memo-last
   (fn [app]
     (into {} (map (fn [type] [(:name type) type])) (:task-types app)))))


;; Task filter helpers:
(defn type-filter [task-type]
  (filter #(= (:type %) task-type)))

(defn since
  "Returns a transducer that returns only tasks that started after the
  given time stamp. Assumes that the input is sorted by start time,
  descending."
  [stamp]
  (take-while #(> (:started %) stamp)))

(defn before
  "Returns a transducer that returns only tasks that started before the
  given time stamp. Assumes that the input is sorted by start time,
  descending."
  [stamp]
  (drop-while #(> (:started %) stamp)))

(defn between [start end]
  (comp (before (.valueOf end)) (since (.valueOf start))))

(def group-days
  (partition-by #($/day-components (js/Date. (:started %)))))

(defn for-day
  "Returns a transducer that filters an ordered input of tasks and
  outputs tasks that were started on the given day."
  [d]
  (let [start($/start-of-day d)
        end ($/inc-date start)]
    (between start end)))


(defn recent-types
  "Retrieves the n most recent task types as maps. Assumes that the
  entries in the :work sequence are sorted by start date, with most
  recent first."
  [app n]
  (sequence (comp (map :type)
                  (distinct)
                  (take n)
                  (map (task-map app)))
            (:work app)))
