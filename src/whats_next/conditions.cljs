(ns whats-next.conditions
  "Condition format:

  [:time "
  (:require [whats-next.state :as s]
            [whats-next.utils :as $]))

(defn condition-active? [])

(defn active-conditions
  "Calculate or retrieve the currently active goal conditions."
  [app]
  ;; TODO: Complete this implementation
  (:goals app))

(defn add-condition [app goal]
  (assoc app :goals (conj (:goals app []) goal)))

(defmulti make-condition
  "Convert a condition description into a reducing function."
  first)

;; Possible time conditions:
;;  <keyword> - in the past interval
;;  [<#> <keyword>] in the past interval

(defmethod make-condition :time
  [[_ when]]
  (cond (keyword? when)
        ;; Recurring goals:
        (case when
          :today (s/for-today)
          :week (s/for-week)
          :month (s/for-month)
          :year (s/for-year))

        ;;
        (vector? when)
        (let [[w1 w2] when]
          (if (keyword? w2)
            ;; Recurring goal
            (s/since ($/inc-type
                      ((case w2
                         :day $/start-of-day
                         :week $/start-of-week
                         :month $/start-of-month
                         :year $/start-of-year))
                      w2 (- (dec w1))))

            (s/between w1 w2)))))

(defmethod make-condition :type
  [[_ t]]
  (s/type-filter t))

(defn make-conditions-reducer
  "Takes a sequence of conditions and returns a reducer function"
  [conds]
  (apply comp (map make-condition conds)))
