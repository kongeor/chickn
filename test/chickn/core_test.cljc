(ns chickn.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [eval-pop crossover roulette
                                 breed-pop mutate raw-pop->pop
                                 gen-pop swap-mutate val-cycle
                                 ordered-crossover]]))

(deftest eval-pop-test
  (let [pop {:pop [{:genes [0 0 1 1] :age 1}
                   {:genes [0 0 1 2] :age 2}
                   {:genes [0 1 0 0]}]}
        cfg {:fitness (fn [genes] (apply + genes))}]
    (is (= {:pop-avg 2.0
            :std-dev 1.0
            :age-avg 2.0
            :best-fitness 3
            :iteration 1
            :best-chromo [0 0 1 2]
            :pop [{:genes [0 0 1 2]
                   :fitness 3
                   :age 3}
                  {:genes [0 0 1 1]
                   :fitness 2
                   :age 2}
                  {:genes [0 1 0 0]
                   :fitness 1
                   :age 1}]}
           (eval-pop cfg pop)))))

(deftest ordered-crossover-test
  (testing "ordered crossover"
    (let [c1 {:genes [4 1 2 3 5 6]}
          c2 {:genes [3 5 4 6 2 1]}
          rf (val-cycle 2 5)]
      (is (= [1 4 2 3 5 6]
             (ordered-crossover rf c1 c2))))))

(deftest crossover-test
  (testing "one pointcut"
    (let [c1 {:genes [1 2 3 4]}
          c2 {:genes [5 6 7 8]}
          ps 1
          rf (fn [& _] 2)]
      (is (= [{:genes [1 2 7 8] :age 0} {:genes [5 6 3 4] :age 0}]
             (crossover rf ps c1 c2)))))
  (testing "two pointcuts"
    (let [c1 {:genes [1 2 3 4 5]}
          c2 {:genes [5 6 7 8 9]}
          ps 2
          rf (let [i (atom 0)]
               (fn [& _] (swap! i + 2)))]
      (is (= [{:genes [1 2 7 8 5] :age 0} {:genes [5 6 3 4 9] :age 0}]
             (crossover rf ps c1 c2))))))

(deftest swap-mutate-test
  (testing "Swapping 2 genes"
    (let [rf (val-cycle 2 4)]
      (is (= [1 2 5 4 3 6]
             (swap-mutate rf [1 2 3 4 5 6]))))))

(deftest roulette-test
  (let [pop {:pop [{:genes   [0 0 1 2]
                    :fitness 3}
                   {:genes   [0 0 1 1]
                    :fitness 2}
                   {:genes   [0 1 0 0]
                    :fitness 1}]}]
    (testing "roulette matching"
      (let [rf (let [i (atom 0)]
                 (fn [& _]
                   (if (= 0 @i)
                     (do
                       (swap! i inc)
                       0.5)
                     0.9)))]
        (is {:genes [0 0 1 1] :fitness 2}
            (roulette {:random-func rf} pop))))
    (testing "fallback"
      (let [rf (let [i (atom 0)]
                 (fn [& _]
                   (if (= 0 @i)
                     (do
                       (swap! i inc)
                       0.99999)
                     0.1)))]
        (is {:genes [0 0 1 1] :fitness 3}
            (roulette {:random-func rf} pop))))))

(deftest mutation-test
  (let [pop {:pop [{:genes [0 0 0 0] :age 5}]}
        rf (let [i (atom 0)]
             (fn [& _]
               (swap! i inc)
               (if (= 2 @i)
                 0.1
                 0.9)))
        mf (constantly 1)]
    (is (= {:pop [{:genes [0 1 0 0] :age 5}]}
           (mutate {:mutation-rate 0.5
                    :random-func rf
                    :mutation-func mf
                    :elitism-rate 0} pop)))))

(deftest breed-pop-test
  (let [pop {:pop [{:genes   [2 2 0 0]
                    :fitness 4}
                   {:genes   [0 0 1 2]
                    :fitness 3}
                   {:genes   [0 0 1 1]
                    :fitness 2}
                   {:genes   [0 1 0 0]
                    :fitness 1}]}]
    (testing "breeding"
      (let [rf (let [i (atom 0)]
                 (fn [& _]
                   (if (= 0 (mod (swap! i inc) 2))
                     2
                     0.5)))
            crossover (partial crossover rf 1)]
        ;; FIXME
        (is (= {:pop [{:genes [2 2 0 0] :age 0}
                      {:genes [0 0 1 2] :fitness 3}
                      {:genes [0 0 1 1] :age 0}
                      {:genes [0 1 0 0] :fitness 1}]}
               (breed-pop {:pop-size       4
                           :elitism-rate   0
                           :random-func    rf
                           :crossover-rate 1
                           :crossover      crossover} pop)))))))

(deftest raw-pop->pop-test
  (testing "Conversion"
    (let [pop [[2 2 0 0]
               [0 0 1 2]
               [0 0 0 0]
               [0 1 0 0]]]
      (is (= {:pop [{:genes [2 2 0 0] :fitness 0 :age 0}
                    {:genes [0 0 1 2] :fitness 0 :age 0}
                    {:genes [0 0 0 0] :fitness 0 :age 0}
                    {:genes [0 1 0 0] :fitness 0 :age 0}]}
             (raw-pop->pop pop))))))

(deftest gen-pop-test
  (testing "Genereting a random pop"
    (is (= [[1 1 1 1]
            [1 1 1 1]
            [1 1 1 1]]
           (gen-pop 3 4 (constantly 1))))))
