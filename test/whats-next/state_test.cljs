(ns whats-next.state-test
  (:require [cljs.test :refer-macros [deftest is testing]]

            [whats-next.state :as state :refer [add-task clean-state current-task
                                                last-type start-task]]))

(deftest test-add-1
  (is (= (-> (clean-state)
             (add-task {:type "Testing"
                        :started 1000000000
                        :ended   2000000000})
             (last-type))
         "Testing")))

(deftest test-goto
  (is (= [:main]
         (-> (clean-state)
             (goto :main)
             (:view-stack)
             (peek)))))

(deftest test-start
  (is (= (-> (clean-state)
             (start-task "Testing")
             (current-task)
             :type)
         "Testing")))
