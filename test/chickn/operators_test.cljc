(ns chickn.operators-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [genes->chromo]]
            [chickn.operators :refer [cut-crossover ordered-crossover
                                      swap-mutate ->operator]]
            [chickn.util :refer [val-cycle]]))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 (genes->chromo [1 2 3 4])
          c2 (genes->chromo [5 6 7 8])
          rf (constantly 2)]
      (is (= [{:genes [1 2 7 8] :fitness 0 :age 0} {:genes [5 6 3 4] :fitness 0 :age 0}]
             ((cut-crossover {:chickn.operators/random-point rf}) c1 c2))))))

(deftest ordered-crossover-test
  (testing "ordered crossover"
    (let [c1 {:genes [4 1 2 3 5 6] :fitness 0 :age 0}
          c2 {:genes [3 5 4 6 2 1] :fitness 0 :age 0}
          rf (val-cycle 2 5)]
      (is (= {:genes [1 4 2 3 5 6] :fitness 0 :age 0}
             ((ordered-crossover {:chickn.operators/random-point rf}) c1 c2))))))

(deftest swap-mutate-test
  (testing "Swapping 2 genes"
    (let [rf (val-cycle 2 4)]
      (is (= {:genes [1 2 5 4 3 6] :fitness 0 :age 0}
             (swap-mutate rf (genes->chromo [1 2 3 4 5 6])))))))


(deftest crossover-pop-test
  (testing "crossover pop wiring"
    (with-redefs [shuffle identity]                         ;; TODO check if needed
      (let [genotype (chickn.core/raw-pop->pop (partition 4 (range 16))) ;; FIXME
            chromos (:pop genotype)
            rnd-chromos (val-cycle (first chromos) (second chromos) (nth chromos 2) (nth chromos 3))
            cfg {:chickn.core/pop-size 4}
            rf (constantly 0)]
        (is (= [{:genes [0 1 6 7] :fitness 0 :age 0}
                {:genes [4 5 2 3] :fitness 0 :age 0}
                {:genes [8 9 14 15] :fitness 0 :age 0}
                {:genes [12 13 10 11] :fitness 0 :age 0}]
               ((->operator #:chickn.operators{:type         :chickn.operators/cut-crossover
                                               :rate         1.0
                                               :pointcuts    1
                                               :rand-nth rnd-chromos
                                               :random-point (fn [& _] 2)
                                               :random-func  rf}) cfg pop)))))))

(deftest ordered-crossover-pop-test
  (testing "ordered crossover wiring"
    (with-redefs [shuffle identity]
      (let [pop (chickn.core/raw-pop->pop [[1 2 3 4 5 6]
                                           [2 3 4 5 6 1]
                                           [4 1 5 3 2 6]
                                           [6 3 2 1 4 5]])
            chromos (:pop pop)
            rnd-chromos (val-cycle (first chromos) (second chromos)
                                   (nth chromos 2) (nth chromos 3)
                                   (second chromos) (first chromos)
                                   (nth chromos 3) (nth chromos 2))
            cfg {:chickn.core/pop-size 4}
            rf (val-cycle 2 4 1 3)]
        (is (= [{:genes [6 1 3 4 2 5] :fitness 0 :age 0}
                {:genes [4 1 5 6 3 2] :fitness 0 :age 0}
                {:genes [6 1 4 5 2 3] :fitness 0 :age 0}
                {:genes [6 3 2 4 1 5] :fitness 0 :age 0}]
               ((->operator #:chickn.operators{:type         :chickn.operators/ordered-crossover
                                               :rate         1.0
                                               :pointcuts    1
                                               :random-point rf
                                               :rand-nth rnd-chromos}) cfg pop)))))))

(deftest swap-mutate-pop-test
  (testing "swap mutate pop wiring"
    (with-redefs [shuffle identity]
      (let [pop (chickn.core/raw-pop->pop (partition 4 (range 16))) ;; FIXME
            chromos (:pop pop)
            rnd-genes (val-cycle 1 3)
            cfg {}
            rf (val-cycle 0.5 0 0.5 0.5)]
        (is (= [{:genes [0 1 2 3] :fitness 0 :age 0}
                {:genes [4 7 6 5] :fitness 0 :age 0}
                {:genes [8 9 10 11] :fitness 0 :age 0}
                {:genes [12 13 14 15] :fitness 0 :age 0}]
               ((->operator #:chickn.operators{:type         :chickn.operators/swap-mutation
                                               :rate         0.3
                                               :rand-nth rnd-genes
                                               :random-func  rf}) cfg chromos)))))))
