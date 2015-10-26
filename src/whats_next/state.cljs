(ns whats-next.state
  "Functions for manipulating the application state."
  (:require [om.core :as om]

            [whats-next.utils :as $]))

(defn get-type [app n]
  ($/find-pred #(= n (:name %)) (:task-types app)))

(defn last-task [app]
  (-> app :work pop))

(def last-type (comp :type last-task))

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

(defn end-task [task]
  (assoc task :ended (.valueOf (js/Date.))))

(defn add-task [app task]
  (assoc app
         :work (conj (:work app) task)))

(defn edit-last [{[last-work & work] :work :as app} f]
  (assoc app :work (conj work (f last-work))))

(defn complete
  "Mark the current task as complete."
  [app]
  (let [task (:current-task app)
        type-name (:type task)
        task-type (get-type app type-name)]
    (assoc (add-task app (end-task task))
      :current-task nil
      :view-stack (pop (:view-stack app)))))

(defn cancel
  "Cancel the current task."
  [app]
  (assoc app
         :current-task nil
         ;; Save so that the task can be restored:
         :canceled-task (end-task (:current-task app))
         :view-stack (pop (:view-stack app))))

(defn restore-canceled
  "Moves the most recently canceled task to the work log."
  [app]
  (-> app
      (add-task (:canceled-task app))
      (dissoc :canceled-task)))

(defn clear-canceled [app]
  (dissoc app :canceled-task))

(defn restart
  "Restart the current task."
  [app]
  (assoc-in app [:current-task :started] (.valueOf (js/Date.))))

(defn edit-log [app trans]
  (assoc app :work (into [] trans (:work app))))



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

(def task-map
  ($/memo-last
   (fn [app]
     (into {} (map (fn [type] [(:name type) type])) (:task-types app)))))

;; Task filter helpers:
(defn type-filter [task-type]
  (cond (string? task-type)
        (filter #(= (:type %) task-type))

        (set? task-type)
        (filter task-type)))

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

(defn group-totals
  "Returns a transducer that produces pairs of the form:

  [(label-fn (first group)) <total duration>]"
  [label-fn]
  (comp (map (fn [group]
               (when-let [f (first group)]
                 [(label-fn f) (total-duration group)])))
        (remove nil?)))

(defn for-day
  "Returns a transducer that filters an ordered input of tasks and
  outputs tasks that were started on the given day."
  ([d & [n]]
   (let [start ($/start-of-day d)
         end ($/inc-date start (or n 1))]
     (between start end)))
  ([]
   (for-day ($/now))))

(defn for-week
  ([d]
   (let [start ($/start-of-week d)]
     (between start ($/inc-week start ))))
  ([]
   (for-week ($/now))))

(defn for-month
  ([d]
   (let [start ($/start-of-month d)]
     (between start ($/inc-month start))))
  ([]
   (for-month ($/now))))

(defn for-year
  ([d]
   (let [start ($/start-of-year d)]
     (between start ($/inc-year start))))
  ([]
   (for-year ($/now))))

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

(defn gap? [task]
  (= (:type task) :gap))

(defn with-gaps
  "Returns a new transducer that will insert special 'gap' tasks
  between tasks."
  []
  (comp ($/staggered (fn [ntask task]
                       (if ntask
                         [{:type :gap
                           :started (:ended task)
                           :ended (:started ntask)}
                          task]
                         [task])))
        (mapcat identity)))

;; TODO: It would be nice to (attempt to) rewrite this as a transducer,
;; in case I eventually switch to asynchronous loading of tasks
(defn day-groups-contiguous
  "Group tasks by the day on which they were started. Insert an empty
  vector for each missing day. Each group is eagerly calculated, but the
  overall sequence is lazy."
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
  "Returns a sequence of sets corresponding "
  ([work]
   (sequence (comp
              group-days
              (map #(into #{} (map :type) %)))
             work)))

(defn group-contiguous
  ([work f n]
   (lazy-seq
    (when-let [tasks (seq work)]
      (let [m-tasks (take-while #(= (f %) n) tasks)]
        (when (seq m-tasks)
          (cons (vec m-tasks) (group-contiguous
                               (drop (count m-tasks) tasks)
                               f
                               (inc n))))))))
  ([work f]
   (group-contiguous work f 0)))

(defn group-contiguous-days
  ([work ref-date]
   (let [stamp ($/->stamp ref-date)]
     (group-contiguous work
                       (fn [job]
                         (quot (- stamp (:started job)) 86400000)))))
  ([work]
   (group-contiguous-days work ($/end-of-day (js/Date. (:started (first work)))))))

(defn daily-totals
  ([work ref-date]
   (sequence (group-totals #(-> % :started js/Date. $/day-components))
             (group-contiguous-days work ref-date)))
  ([work]
   (daily-totals work ($/end-of-day ($/now)))))

;; Managing goals

;; Goal transducers:
;;
(defn rename-task
  "Returns a transducer that "
  [old-name new-name]
  (map (fn [task]
         (if (= (:type task) old-name)
           (assoc task :type new-name)
           task))))
