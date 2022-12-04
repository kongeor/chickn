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
  #:chickn.core{:chromo-gen      #(repeatedly rastrigin-vars rast-rnd)
                :population-size pop-size
                :solved?         util/noop
                :monitor         util/noop
                :fitness         fitness
                :comparator      chickn/lower-is-better
                :reporter        util/simple-printer
                :selector        #:chickn.selector{:type        :chickn.selector/roulette
                                                   :rate        0.3
                                                   :random-func rand}
                :crossover       #:chickn.crossover{:type         :chickn.crossover/cut-crossover
                                                    :rate         0.3
                                                    :pointcuts    1
                                                    :random-point math/rnd-index
                                                    :random-func  rand
                                                    :rand-nth     rand-nth}
                :mutation        #:chickn.mutation{:type          :chickn.mutation/rand-mutation
                                                   :rate          0.1
                                                   :random-func   rand
                                                   :mutation-func rast-rnd}
                :reinsertion     #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                                      :rate 0.1}})

#_(clojure.spec.alpha/explain-data :chickn.core/config rast-cfg)

(comment
  (chickn/init-and-evolve rast-cfg 5000))
