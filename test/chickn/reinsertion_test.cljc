(ns chickn.reinsertion-test
  (:require [clojure.test :refer [deftest is testing]]
            [chickn.core :refer [higher-is-better]]
            [chickn.reinsertion :refer [elitist]]))

(deftest elitist-test
  (testing "one pointcut"
    (let [parents   [{:genes [1 1 1 1] :fitness 1 :age 0}
                     {:genes [2 2 2 2] :fitness 4 :age 0}
                     ]
          offspring [{:genes [1 1 2 2] :fitness 5 :age 0}
                     {:genes [2 2 1 1] :fitness 5 :age 0}]
          leftover  [{:genes [3 3 3 3] :fitness 3 :age 0}
                     {:genes [4 4 4 4] :fitness 2 :age 0}]]
      (is (= [{:genes [2 2 2 2] :fitness 4 :age 0}
              {:genes [3 3 3 3] :fitness 3 :age 0}
              {:genes [1 1 2 2] :fitness 5 :age 0}
              {:genes [2 2 1 1] :fitness 5 :age 0}]
             ((elitist {:chickn.reinsertion/rate 0.5})
              {:chickn.core/comparator higher-is-better :chickn.core/population-size 4}
              {:parents   parents
               :offspring offspring
               :leftover  leftover})))))
  )
