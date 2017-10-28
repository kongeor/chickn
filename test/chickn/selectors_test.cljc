(ns chickn.selectors-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [val-cycle]]
            [chickn.selectors :refer [->selector]]))

(deftest routette-test
  (testing "roulette fairness"
    (let [raw-pop [{:genes [0 1 2 3] :fitness 4}
                   {:genes [4 5 2 3] :fitness 3}
                   {:genes [8 9 2 3] :fitness 2}
                   {:genes [12 13 2 3] :fitness 1}]
          pop {:pop raw-pop}
          cfg {:chickn.core/comparator chickn.core/descending}
          make-roulette (fn [random-func]
                          (->selector #:chickn.selectors{:type :chickn.selectors/roulette
                                                         :random-func random-func}))]
      (with-redefs [shuffle identity]
        (let [random-func (constantly 0.16)]
          (is (= {:genes [0 1 2 3] :fitness 4}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.51)]
          (is (= {:genes [4 5 2 3] :fitness 3}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.71)]
          (is (= {:genes [8 9 2 3] :fitness 2}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.96)]
          (is (= {:genes [12 13 2 3] :fitness 1}
                 ((make-roulette random-func) pop cfg)))))))
  (testing "roulette fairness inverse"
    (let [raw-pop [{:genes [12 13 2 3] :fitness 1}
                   {:genes [8 9 2 3] :fitness 2}
                   {:genes [4 5 2 3] :fitness 3}
                   {:genes [0 1 2 3] :fitness 4}]
          pop {:pop raw-pop}
          cfg {:chickn.core/comparator chickn.core/ascending}
          make-roulette (fn [random-func]
                          (->selector #:chickn.selectors{:type :chickn.selectors/roulette
                                                         :random-func random-func}))]
      (with-redefs [shuffle identity]
        ;; inverse probability will give this a zero selection chance
        #_(let [random-func (constantly 0.16)]
          (is (= {:genes [0 1 2 3] :fitness 4}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.9)]
          (is (= {:genes [4 5 2 3] :fitness 3}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.71)]
          (is (= {:genes [8 9 2 3] :fitness 2}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.42)]
          (is (= {:genes [12 13 2 3] :fitness 1}
                 ((make-roulette random-func) pop cfg))))))))
