(ns chickn.crossover-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [genes->chromo]]
            [chickn.crossover :refer [cut-crossover ordered-crossover
                                     ->crossover]]
            [chickn.util :refer [val-cycle]]))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 (genes->chromo [1 2 3 4])
          c2 (genes->chromo [5 6 7 8])
          rf (constantly 2)]
      (is (= [{:genes [1 2 7 8] :fitness 0 :age 0} {:genes [5 6 3 4] :fitness 0 :age 0}]
             ((cut-crossover {:chickn.crossover/random-point rf}) c1 c2))))))

(deftest ordered-crossover-test
  (testing "ordered crossover"
    (let [c1 {:genes [4 1 2 3 5 6] :fitness 0 :age 0}
          c2 {:genes [3 5 4 6 2 1] :fitness 0 :age 0}
          rf (val-cycle 2 5)]
      (is (= {:genes [1 4 2 3 5 6] :fitness 0 :age 0}
             ((ordered-crossover {:chickn.crossover/random-point rf}) c1 c2))))))

(deftest crossover-pop-test
  (testing "crossover pop wiring"
    (with-redefs [shuffle identity]                         ;; TODO check if needed
      (let [pop (chickn.core/raw-pop->pop (partition 4 (range 16))) ;; FIXME
            chromos (:chromosomes pop)
            rnd-chromos (val-cycle (first chromos) (second chromos) (nth chromos 2) (nth chromos 3))
            cfg {:chickn.core/population-size 4}
            rf (constantly 0)]
        (is (= [{:genes [0 1 6 7] :fitness 0 :age 0}
                {:genes [4 5 2 3] :fitness 0 :age 0}
                {:genes [8 9 14 15] :fitness 0 :age 0}
                {:genes [12 13 10 11] :fitness 0 :age 0}]
               ((->crossover #:chickn.crossover{:type         :chickn.crossover/cut-crossover
                                               :rate         1.0
                                               :pointcuts    1
                                               :rand-nth     rnd-chromos
                                               :random-point (fn [& _] 2)
                                               :random-func  rf}) cfg pop)))))))

(deftest ordered-crossover-pop-test
  (testing "ordered crossover wiring"
    (with-redefs [shuffle identity]
      (let [pop (chickn.core/raw-pop->pop [[1 2 3 4 5 6]
                                           [2 3 4 5 6 1]
                                           [4 1 5 3 2 6]
                                           [6 3 2 1 4 5]])
            chromos (:chromosomes pop)
            rnd-chromos (val-cycle (first chromos) (second chromos)
                                   (nth chromos 2) (nth chromos 3)
                                   (second chromos) (first chromos)
                                   (nth chromos 3) (nth chromos 2))
            cfg {:chickn.core/population-size 4}
            rf (val-cycle 2 4 1 3)]
        (is (= [{:genes [6 1 3 4 2 5] :fitness 0 :age 0}
                {:genes [4 1 5 6 3 2] :fitness 0 :age 0}
                {:genes [6 1 4 5 2 3] :fitness 0 :age 0}
                {:genes [6 3 2 4 1 5] :fitness 0 :age 0}]
               ((->crossover #:chickn.crossover{:type        :chickn.crossover/ordered-crossover
                                               :rate         1.0
                                               :pointcuts    1
                                               :random-point rf
                                               :rand-nth     rnd-chromos}) cfg pop)))))))

