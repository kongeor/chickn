(ns chickn.mutation-test
  (:require [chickn.chromosome :refer [genes->chromo]]
            [chickn.mutation :refer [swap-mutate ->mutation]]
            [chickn.util :refer [val-cycle]]
            [clojure.test :refer [deftest is testing]]))

(deftest swap-mutate-test
  (testing "Swapping 2 genes"
    (let [rf (val-cycle 2 4)]
      (is (= {:genes [1 2 5 4 3 6] :fitness 0 :age 0}
             (swap-mutate rf (genes->chromo [1 2 3 4 5 6])))))))

(deftest swap-mutate-pop-test
  (testing "swap mutate pop wiring"
    (with-redefs [shuffle identity]
      (let [pop (chickn.core/raw-pop->pop (partition 4 (range 16))) ;; FIXME
            chromos (:chromosomes pop)
            rnd-genes (val-cycle 1 3)
            cfg {}
            rf (val-cycle 0.5 0 0.5 0.5)]
        (is (= [{:genes [0 1 2 3] :fitness 0 :age 0}
                {:genes [4 7 6 5] :fitness 0 :age 0}
                {:genes [8 9 10 11] :fitness 0 :age 0}
                {:genes [12 13 14 15] :fitness 0 :age 0}]
               ((->mutation #:chickn.mutation{:type         :chickn.mutation/swap-mutation
                                              :rate        0.3
                                              :rand-nth    rnd-genes
                                              :random-func rf}) cfg chromos)))))))
