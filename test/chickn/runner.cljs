(ns chickn.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [chickn.core-test]
            [chickn.operators-test]
            [chickn.selectors-test]))

(doo-tests 'chickn.core-test
           'chickn.operators-test
           'chickn.selectors-test)