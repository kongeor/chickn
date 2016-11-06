(ns chickn.selectors-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [val-cycle]]
            [chickn.selectors :refer [selector]]))

(deftest routette-test
  (testing "roulette fairness"
    (let [pop [{:genes [0 1 2 3] :fitness 1}
               {:genes [4 5 2 3] :fitness 1}
               {:genes [8 9 2 3] :fitness 1}
               {:genes [12 13 2 3] :fitness 1}]
          make-roulette (fn [random-func]
                          (selector #:chickn.selectors{:type :chickn.selectors/roulette
                                                       :random-func random-func}))]
      (with-redefs [shuffle identity]
        (let [random-func (constantly 0.16)]
          (is (= {:genes [0 1 2 3] :fitness 1}
                 ((make-roulette random-func) pop))))
        (let [random-func (constantly 0.26)]
          (is (= {:genes [4 5 2 3] :fitness 1}
                 ((make-roulette random-func) pop))))
        (let [random-func (constantly 0.51)]
          (is (= {:genes [8 9 2 3] :fitness 1}
                 ((make-roulette random-func) pop))))
        (let [random-func (constantly 0.76)]
          (is (= {:genes [12 13 2 3] :fitness 1}
                 ((make-roulette random-func) pop))))))))
