(ns chickn.operators-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [val-cycle]]
            [chickn.operators :refer [cut-crossover]]))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 [1 2 3 4]
          c2 [5 6 7 8]
          rf (constantly 2)]
      (is (= [[1 2 7 8] [5 6 3 4]]
             ((cut-crossover {:chickn.operators/random-point rf}) c1 c2))))))
