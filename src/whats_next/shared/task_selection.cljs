(ns whats-next.shared.task-selection
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]

            [whats-next.utils :as $]))

(defn task-selector [app f]
  (apply dom/select
         #js {:onChange (fn [e]
                          (f (.. e -target -value))
                          ($/cancel e))
              :value task-type}
         (for [{n :name} (:task-types app)]
           (dom/option #js {:value n} n))))

#_
(defn goals-today [])
