(ns chickn.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [chickn.core-test]))

(doo-tests 'chickn.core-test)