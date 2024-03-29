(ns chickn.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [chickn.core :refer [eval-pop raw-pop->pop]]))

(deftest eval-pop-test
  (let [pop {:chromosomes [{:genes [0 0 1 1] :age 1}
                           {:genes [0 0 1 2] :age 2}
                           {:genes [0 1 0 0]}]}
        cfg {:chickn.core/fitness (fn [genes] (apply + genes))
             :chickn.core/monitor chickn.util/noop
             :chickn.core/comparator chickn.core/higher-is-better}]
    (is (= {:pop-avg 2.0
            :std-dev 1.0
            :age-avg 2.0
            :best-fitness 3
            :iteration 1
            :total-fitness 6
            :best-chromosome [0 0 1 2]
            :chromosomes [{:genes [0 0 1 2]
                           :fitness 3
                           :age 3}
                          {:genes [0 0 1 1]
                           :fitness 2
                           :age 2}
                          {:genes [0 1 0 0]
                           :fitness 1
                           :age 1}]}
          (dissoc (eval-pop cfg pop) :time)))))

#_(deftest crossover-test
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

#_(deftest roulette-test
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

#_(deftest mutation-test
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

#_(deftest breed-pop-test
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
      (is (= {:chromosomes [{:genes [2 2 0 0] :fitness 0 :age 0}
                            {:genes [0 0 1 2] :fitness 0 :age 0}
                            {:genes [0 0 0 0] :fitness 0 :age 0}
                            {:genes [0 1 0 0] :fitness 0 :age 0}]}
             (raw-pop->pop pop))))))

#_(deftest gen-pop-test
  (testing "Genereting a random pop"
    (is (= [[1 1 1 1]
            [1 1 1 1]
            [1 1 1 1]]
           (gen-pop 3 4 (constantly 1))))))
