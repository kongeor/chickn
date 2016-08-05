(ns chickn.core-test
  (:require [clojure.test :refer :all]
            [chickn.core :refer :all]))

(deftest rank-test
  (let [pop [[0 0 1 1]
             [0 0 1 2]
             [0 1 0 0]]
        cfg {:fitness (fn [genes] (apply + genes))}]
    (is (= 0
           (rank cfg pop)))))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 [1 2 3 4]
          c2 [5 6 7 8]
          ps 1
          rf (fn [& _] 2)]
      (is (= [[1 2 7 8] [5 6 3 4]]
             (crossover rf ps c1 c2)))))
  (testing "two pointcuts"
    (let [c1 [1 2 3 4 5]
          c2 [5 6 7 8 9]
          ps 2
          rf (let [i (atom 0)]
               (fn [& _] (swap! i + 2)))]
      (is (= [[1 2 7 8 5] [5 6 3 4 9]]
             (crossover rf ps c1 c2))))))
