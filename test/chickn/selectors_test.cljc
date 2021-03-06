(ns chickn.selectors-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.selectors :refer [->selector]]
            [chickn.util :as util]))

(deftest routette-test
  (testing "roulette fairness"
    (let [pop [{:genes [0 1 2 3] :fitness 4}
                   {:genes [4 5 2 3] :fitness 3}
                   {:genes [8 9 2 3] :fitness 2}
                   {:genes [12 13 2 3] :fitness 1}]
          cfg {:chickn.core/comparator chickn.core/higher-is-better}
          make-roulette (fn [random-func]
                          (->selector #:chickn.selectors{:type :chickn.selectors/roulette
                                                         :random-func random-func}))]
      (with-redefs [shuffle identity]
        (let [random-func (constantly 0.16)]
          (is (= [{:genes [0 1 2 3] :fitness 4}]
                 ((make-roulette random-func) cfg pop 1))))
        (let [random-func (constantly 0.51)]
          (is (= [{:genes [4 5 2 3] :fitness 3}]
                 ((make-roulette random-func) cfg pop 1))))
        (let [random-func (constantly 0.71)]
          (is (= [{:genes [8 9 2 3] :fitness 2}]
                 ((make-roulette random-func) cfg pop 1))))
        (let [random-func (constantly 0.96)]
          (is (= [{:genes [12 13 2 3] :fitness 1}]
                 ((make-roulette random-func) cfg pop 1)))))))
  (testing "roulette fairness inverse"
    (let [pop [{:genes [12 13 2 3] :fitness 1}
                   {:genes [8 9 2 3] :fitness 2}
                   {:genes [4 5 2 3] :fitness 3}
                   {:genes [0 1 2 3] :fitness 4}]
          cfg {:chickn.core/comparator chickn.core/lower-is-better}
          make-roulette (fn [random-func]
                          (->selector #:chickn.selectors{:type :chickn.selectors/roulette
                                                         :random-func random-func}))]
      (with-redefs [shuffle identity]
        ;; inverse probability will give this a zero selection chance
        ;; FIXME
        #_(let [random-func (constantly 0.16)]
          (is (= {:genes [0 1 2 3] :fitness 4}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.9)]
          (is (= [{:genes [4 5 2 3] :fitness 3}]
                 ((make-roulette random-func) cfg pop 1))))
        (let [random-func (constantly 0.71)]
          (is (= [{:genes [8 9 2 3] :fitness 2}]
                 ((make-roulette random-func) cfg pop 1))))
        (let [random-func (constantly 0.42)]
          (is (= [{:genes [12 13 2 3] :fitness 1}]
                 ((make-roulette random-func) cfg pop 1))))))))
