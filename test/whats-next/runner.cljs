(ns whats-next.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [whats-next.state-test]))

(doo-tests 'whats-next.state-test)
