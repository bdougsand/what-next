(ns whats-next.conditions
  "Goal format:

  {:conditions [Condition],
   :total milliseconds }

Condition format:

  [:time :daily] - this is a daily goal. Other valid
  keywords: :weekly, :monthly, etc.

  [:time [start-time end-time]]

  [:type keyword] - goal applies to tasks of a particular type"
  (:require [whats-next.state :as s]
            [whats-next.utils :as $]))

(defn condition-active? [[condition-type condition]]
  (or (not= condition-type :time)
      (or (keyword? condition)
          (and (vector? condition)
               (< (second condition) ($/->stamp ($/now)))))))

(defn goal-active? [goal]
  (every? condition-active? (:conditions goal)))

(defn active-conditions
  "Calculate or retrieve the currently active goal conditions."
  [app]
  (:active-goals app))

(defn cleanup-conditions
  "Remove conditions that are no longer active, recording whether they
  were successfully completed or not."
  [app]
  ())

(defn add-condition [app goal]
  (assoc app :active-goals (conj (:active-goals app []) goal)))

(defmulti make-condition
  "Convert a condition description into a reducing function."
  first)

(def name-for-recurring
  {{:daily "every day"
    :weekly "every week"
    :monthly "every month"
    :yearly "every year"}})

;; Possible time conditions:
;;  <keyword> - in the past interval
;;  [<#> <keyword>] in the past interval

(defmethod make-condition :time
  [[_ when]]
  (cond (keyword? when)
        ;; Recurring goals:
        (case when
          :daily (s/for-today)
          :weekly (s/for-week)
          :monthly (s/for-month)
          :yearly (s/for-year))

        (vector? when)
        (s/between (first when) (second when))))

(defmethod make-condition :type
  [[_ t]]
  (s/type-filter t))

(defn make-conditions-reducer
  "Takes a sequence of conditions and returns a reducer function"
  [conds]
  (apply comp (map make-condition conds)))
