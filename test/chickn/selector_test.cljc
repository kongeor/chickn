(ns chickn.selector-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.selector :refer [->selector]]
            [chickn.core]
            [chickn.util :as util]))

(deftest roulette-test
  (testing "roulette fairness"
    (let [pop [{:genes [0 1 2 3] :fitness 4}
                   {:genes [4 5 2 3] :fitness 3}
                   {:genes [8 9 2 3] :fitness 2}
                   {:genes [12 13 2 3] :fitness 1}]
          cfg {:chickn.core/comparator chickn.core/higher-is-better :chickn.core/population-size 4}
          make-roulette (fn [random-func]
                          (->selector #:chickn.selector{:type         :chickn.selector/roulette
                                                         :random-func random-func
                                                         :rate        0.25}))]
      (with-redefs [shuffle identity]
        (let [random-func (constantly 0.16)]
          (is (= [{:genes [0 1 2 3] :fitness 4}]
                 (:parents ((make-roulette random-func) cfg pop)))))
        (let [random-func (constantly 0.51)]
          (is (= [{:genes [4 5 2 3] :fitness 3}]
                 (:parents ((make-roulette random-func) cfg pop)))))
        (let [random-func (constantly 0.71)]
          (is (= [{:genes [8 9 2 3] :fitness 2}]
                 (:parents ((make-roulette random-func) cfg pop)))))
        (let [random-func (constantly 0.96)]
          (is (= [{:genes [12 13 2 3] :fitness 1}]
                 (:parents ((make-roulette random-func) cfg pop))))))))
  (testing "roulette fairness inverse"
    (let [pop [{:genes [12 13 2 3] :fitness 1}
               {:genes [8 9 2 3] :fitness 2}
               {:genes [4 5 2 3] :fitness 3}
               {:genes [0 1 2 3] :fitness 4}]
          cfg {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/population-size 4}
          make-roulette (fn [random-func]
                          (->selector #:chickn.selector{:type         :chickn.selector/roulette
                                                         :random-func random-func
                                                         :rate        0.25}))]
      (with-redefs [shuffle identity]
        ;; inverse probability will give this a zero selection chance
        ;; FIXME
        #_(let [random-func (constantly 0.16)]
          (is (= {:genes [0 1 2 3] :fitness 4}
                 ((make-roulette random-func) pop cfg))))
        (let [random-func (constantly 0.9)]
          (is (= [{:genes [4 5 2 3] :fitness 3}]
                 (:parents ((make-roulette random-func) cfg pop)))))
        (let [random-func (constantly 0.71)]
          (is (= [{:genes [8 9 2 3] :fitness 2}]
                 (:parents ((make-roulette random-func) cfg pop)))))
        (let [random-func (constantly 0.42)]
          (is (= [{:genes [12 13 2 3] :fitness 1}]
                 (:parents ((make-roulette random-func) cfg pop)))))))))

(deftest tournament-test
  (let [pop         [{:genes [0 1 2 3] :fitness 4}
                     {:genes [4 5 2 3] :fitness 3}
                     {:genes [8 9 2 3] :fitness 2}
                     {:genes [12 13 2 3] :fitness 1}]
        random-func (util/val-cycle 0.0 0.25 0.5 0.25 0.5 0.75)
        selector    (->selector #:chickn.selector{:type         :chickn.selector/tournament
                                                   :random-func random-func
                                                   :tour-size   3
                                                   :rate        0.5})]
    (testing "tournament higher"
      (let [cfg {:chickn.core/comparator chickn.core/higher-is-better :chickn.core/population-size 4}]
        (is (= {:parents [{:genes [0 1 2 3] :fitness 4}
                          {:genes [4 5 2 3] :fitness 3}]
                :leftover [{:genes [8 9 2 3] :fitness 2}
                           {:genes [12 13 2 3] :fitness 1}]}
               (selector cfg pop)))))
    (testing "tournament lower"
      (let [cfg {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/population-size 4}]
        (is (= {:parents [{:genes [8 9 2 3] :fitness 2}
                          {:genes [12 13 2 3] :fitness 1}]
                :leftover [{:genes [0 1 2 3] :fitness 4}
                           {:genes [4 5 2 3] :fitness 3}]}
               (selector cfg pop)))))
    (testing "allowing duplicates"
      (let [selected ((->selector #:chickn.selector{:type       :chickn.selector/tournament
                                                     :tour-size 3
                                                     :rate      0.75})
                      {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/population-size 4} pop)]
        (is (= 3 (-> selected :parents count)))))
    (testing "not allowing duplicates"
      (let [pop [{:genes [0 1 2 3] :fitness 1}
                 {:genes [4 5 2 3] :fitness 2}
                 {:genes [8 9 2 3] :fitness 4}
                 {:genes [12 13 2 3] :fitness 8}]
            random-func (chickn.util/val-cycle 0.0 0.25 0.5 0.25 0.5 0.75 0.8)
            selected ((->selector #:chickn.selector{:type         :chickn.selector/tournament
                                                     :tour-size   3
                                                     :random-func random-func
                                                     :rate        0.75
                                                     :duplicates? false})
                      {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/population-size 4} pop)]
        (is (= 3 (-> selected :parents set count)))))))

(deftest duplicate-test
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 2 3] :fitness 2}
             {:genes [8 9 2 3] :fitness 4}
             {:genes [12 13 2 3] :fitness 8}]
        random-func (chickn.util/val-cycle 0.0 0.25 0.5 0.25 0.5 0.75 0.8)
        selector-cfg #:chickn.selector{:type         :chickn.selector/tournament
                                        :tour-size   3
                                        :random-func random-func
                                        :rate        0.75}
        cfg {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/population-size 4}]
    (testing "default - allowing duplicates"
      (let [selected ((->selector selector-cfg)
                      cfg pop)]
        (is (= 2 (-> selected :parents set count)))))
    (testing "not allowing duplicates"
      (let [selected ((->selector (assoc selector-cfg :chickn.selector/duplicates? false))
                      cfg pop)]
        (is (= 3 (-> selected :parents set count)))))
    (testing "exceeding number of checks"
      (let [selected ((->selector (assoc selector-cfg :chickn.selector/duplicates? true
                                                      :chickn.operator/random-func (chickn.util/val-cycle 0.0 0.25 0.5 0.25 0.5 0.75)))
                      cfg pop)]
        (is (= 2 (-> selected :parents set count)))))))
