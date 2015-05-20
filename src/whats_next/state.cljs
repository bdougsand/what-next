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
    :view-stack (conj (:view-stack app) [:timer])))

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

(defn insert-task
  "Insert a completed task at the appropriate place. Assumes that the
  task was relatively recent."
  [app {s :started :as task}]
  (assoc app
    :work ($/insert-where #(> (:started %) s) task (:work app))))

(defn add-note-to-current
  "Adds a note string to the current task, if any."
  [{ct :current-task :as app} note]
  (assoc app
    :current-task (assoc ct
                    :notes (when ct
                             (conj (:notes ct []) note)))))

;; Navigation
(defn goto-handler
  "Returns an event handler will change the current view to the
  specified view."
  ([cursor view props]
   (cond
    (keyword? view)
    (fn [e]
      (om/transact! cursor :view-stack #(conj % [view props]))
      (.preventDefault e))

    (ifn? view)
    (fn [e]
      (om/transact! cursor (fn [app]
                             (assoc app
                               :view-stack (view app))))
      (.preventDefault e))))
  ([cursor view]
   (goto-handler cursor view nil)))

(defn go-back [app]
  (pop (:view-stack app)))

;; Manipulating tasks
(defn duration
  "Duration, in milliseconds, of a given task."
  [task]
  (- (:ended task) (:started task)))

(defn total-duration [tasks]
  (reduce + (map duration tasks)))

(defn chain-length [tasks type d]
  )

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
  [d]
  (let [stamp ($/->stamp d)]
    (take-while #(> (:started %) stamp))))

(defn before
  "Returns a transducer that returns only tasks that started before the
  given time stamp. Assumes that the input is sorted by start time,
  descending."
  [d]
  (let [stamp ($/->stamp d)]
    (drop-while #(> (:started %) stamp))))

(defn between [start end]
  (comp (before end) (since start)))

(def group-days
  "Transducer that groups tasks according to the day on which they were
  begun."
  (partition-by #($/day-components (js/Date. (:started %)))))

(defn for-day
  "Returns a transducer that filters an ordered input of tasks and
  outputs tasks that were started on the given day."
  [d]
  (let [start ($/start-of-day d)
        end ($/inc-date start)]
    (between start end)))

(defn for-month [d]
  (let [start ($/start-of-month d)]
    (between start ($/inc-month start))))

(defn for-today []
  (for-day ($/now)))

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

(defn today-work [app]
  (sequence (for-day ($/now)) (:work app)))

(defn day-groups [work]
  (group-by #($/day-components
              (js/Date. (:started %)))
            work))

(defn day-groups-contiguous
  "Group tasks by the day on which they were started. Insert an empty
  vector for each missing day. Each group is eager, but the overall
  sequence is lazy."
  ([work ref-date]
   (lazy-seq
    (when-let [work (seq work)]
      (loop [[w & ws :as work] work, matches []]
        (if (and w ($/same-day? (js/Date. (:started w)) ref-date))
          (recur ws (conj matches w))

          (cons matches
                (day-groups-contiguous work ($/dec-date ref-date 1))))))))
  ([work]
   (when-let [w (first work)]
     (day-groups-contiguous work (js/Date. (:started w))))))

(defn daily-tasks
  ([work]
   (sequence (comp
              group-days
              (map #(into #{} (map :type) %)))
             work)))

(defn group-contiguous-days
  [work ref-date]
  (lazy-seq
   (when-let [tasks (seq work)]
     (let [f #($/same-day? ($/->date (:started %)) ref-date)
           date-tasks (take-while f tasks)]
       (cons [ref-date date-tasks]
             (group-contiguous-days (drop (count date-tasks) tasks)
                                    ($/dec-date ref-date 1)))))))
