(ns chickn.examples.rastrigin
  (:require [chickn.core :as chickn]
            [chickn.util :as util]
            [chickn.math :as math]))

(defn rastrigin [& xs]
  (let [n (count xs)]
    (reduce
      (fn [acc x]
        (+ acc (- (* x x) (* 10 (Math/cos (* 2 Math/PI x)))))) (* n 10) xs)))

(defn fitness [c]
  (apply rastrigin c))

(def rast-rnd #(math/rand-between -5.15 5.12))

(def rastrigin-vars 10)

(def pop-size 100)

(def rast-cfg
  #:chickn.core{:chromo-gen   #(repeatedly rastrigin-vars rast-rnd)
                :pop-size     pop-size
                :elitism-rate 0.1
                :terminated?  util/noop
                :monitor      util/noop
                :fitness      fitness
                :comparator   chickn/lower-is-better
                :reporter     util/simple-printer
                :selector     #:chickn.selectors{:type        :chickn.selectors/roulette
                                                 :rate        0.3
                                                 :random-func rand}
                :crossover    #:chickn.operators{:type         :chickn.operators/cut-crossover
                                                 :rate         0.3
                                                 :pointcuts    1
                                                 :random-point math/rnd-index
                                                 :random-func  rand
                                                 :rand-nth     rand-nth}
                :mutation    #:chickn.operators{:type          :chickn.operators/rand-mutation
                                                 :rate          0.1
                                                 :random-func   rand
                                                 :mutation-func rast-rnd}
                :reinsertion #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                                  :rate 0.1}})

#_(clojure.spec.alpha/explain :chickn.core/config rast-cfg)

(comment
  (let [genotype (chickn/init rast-cfg)]
    (chickn/evolve rast-cfg genotype 5000)))
