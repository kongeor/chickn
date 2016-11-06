(ns chickn.operators-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [val-cycle]]
            [chickn.operators :refer [cut-crossover make-selector operator noop]]))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 [1 2 3 4]
          c2 [5 6 7 8]
          rf (constantly 2)]
      (is (= [[1 2 7 8] [5 6 3 4]]
             ((cut-crossover {:chickn.operators/random-point rf}) c1 c2))))))

(deftest crossover-pop-test
  (testing "wiring"
    (with-redefs [make-selector (fn [_] #(first %))]
      (let [pop (partition 4 (range 16))]
        (is (= [[0 1 2 3] [4 5 2 3] [8 9 2 3] [12 13 2 3]]
               ((operator #:chickn.operators{:type         :chickn.operators/cut-crossover
                                             :rate         1.0
                                             :elitism      0.0
                                             :pointcuts    1
                                             :random-point (fn [& _] 2)
                                             :random-func  rand
                                             :selector     noop}) pop)))))))
(crossover-pop-test)
