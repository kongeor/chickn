(ns chickn.operators-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [val-cycle genes->chromo]]
            [chickn.operators :refer [cut-crossover ->operator]]))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 (genes->chromo [1 2 3 4])
          c2 (genes->chromo [5 6 7 8])
          rf (constantly 2)]
      (is (= [{:genes [1 2 7 8] :fitness 0 :age 0} {:genes [5 6 3 4] :fitness 0 :age 0}]
             ((cut-crossover {:chickn.operators/random-point rf}) c1 c2))))))

(deftest crossover-pop-test
  (testing "wiring"
    (with-redefs [shuffle identity]
      (let [pop (chickn.core/raw-pop->pop (partition 4 (range 16))) ;; FIXME
            chromos (:pop pop)
            rnd-chromos (val-cycle (first chromos) (second chromos) (nth chromos 2) (nth chromos 3))
            cfg {:chickn.core/pop-size 4 :chickn.core/elitism-rate 0 :chickn.core/rand-nth rnd-chromos}
            rf (constantly 0)]
        (is (= [{:genes [0 1 6 7] :fitness 0 :age 0}
                {:genes [4 5 2 3] :fitness 0 :age 0}
                {:genes [8 9 14 15] :fitness 0 :age 0}
                {:genes [12 13 10 11] :fitness 0 :age 0}]
               ((->operator #:chickn.operators{:type         :chickn.operators/cut-crossover
                                               :rate         1.0
                                               :pointcuts    1
                                               :random-point (fn [& _] 2)
                                               :random-func  rf}) pop cfg)))))))
(crossover-pop-test)
