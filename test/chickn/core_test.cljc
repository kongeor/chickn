(ns chickn.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [eval-pop crossover roulette breed-pop]]))

(deftest eval-pop-test
  (let [pop {:pop [{:genes [0 0 1 1]}
                   {:genes [0 0 1 2]}
                   {:genes [0 1 0 0]}]}
        cfg {:fitness (fn [genes] (apply + genes))}]
    (is (= {:pop-avg 2
            :std-dev 1.0
            :best-fitness 3
            :best-chromo [0 0 1 2]
            :pop [{:genes [0 0 1 2]
                   :fitness 3}
                  {:genes [0 0 1 1]
                   :fitness 2}
                  {:genes [0 1 0 0]
                   :fitness 1}]}
           (eval-pop cfg pop)))))


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

(deftest roulette-test
  (let [pop [{:genes [0 0 1 2]
              :fitness 3}
             {:genes [0 0 1 1]
              :fitness 2}
             {:genes [0 1 0 0]
              :fitness 1}]]
    (testing "roulette matching"
      (let [rf (let [i (atom 0)]
                 (fn [& _]
                   (if (= 0 @i)
                     (do
                       (swap! i inc)
                       0.5)
                     0.9)))]
        (is {:genes [0 0 1 1] :fitness 2}
            (roulette {:rf rf} pop))))
    (testing "fallback"
      (let [rf (let [i (atom 0)]
                 (fn [& _]
                   (if (= 0 @i)
                     (do
                       (swap! i inc)
                       0.99999)
                     0.1)))]
        (is {:genes [0 0 1 1] :fitness 3}
            (roulette {:rf rf} pop))))))

(deftest breed-pop-test
  (let [pop [{:genes [2 2 0 0]
              :fitness 4}
             {:genes [0 0 1 2]
              :fitness 3}
             {:genes [0 0 1 1]
              :fitness 2}
             {:genes [0 1 0 0]
              :fitness 1}]]
    (testing "breeding"
      (let [rf (let [i (atom 0)]
                 (fn [& _]
                   (if (= 0 (mod (swap! i inc) 2))
                     1
                     2)))
            crossover (partial crossover rf 1)]
        (is (= [[2 2 0 0]
                [0 0 1 2]
                [0 0 0 0]
                [0 1 0 0]]
               (breed-pop {:rf rf
                           :crossover-rate 1
                           :crossover crossover} pop)))))))
