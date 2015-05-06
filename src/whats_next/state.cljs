(ns whats-next.state
  ""
  (:require [whats-next.utils :as $]))

(defn get-type [app n]
  ($/find-pred #(= n (:name %)) (:task-types app)))
