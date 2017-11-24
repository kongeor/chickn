(ns chickn.examples.ones
  (:require [chickn.core :refer [default-cfg init
                                 evolve higher-is-better lower-is-better]]))

(def one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0)))

(def chromo-len 10)

(def chromo-gen #(repeatedly chromo-len one-or-zero))

(def mutation-op
  #:chickn.operators
  {:type          :chickn.operators/rand-mutation
   :rate          0.01
   :random-func   rand
   :mutation-func one-or-zero})

(def cfg-ones-base
  #:chickn.core
  {:chromo-gen #(repeatedly chromo-len one-or-zero)
   :terminated? (fn [c] (every? #(= 1 %) c))
   :fitness #(apply + %)
   :comparator higher-is-better})

(def cfg-zeros-base
  (merge cfg-ones
    #:chickn.core
    {:terminated? (fn [c] (every? #(= 0 %) c))
     :fitness #(apply + %)
     :comparator lower-is-better}))


(def cfg-ones
  (-> default-cfg
    (merge cfg-ones-base)
    (update-in [:chickn.core/operators] conj mutation-op)))

(def cfg-zeros
  (-> default-cfg
    (merge cfg-zeros-base)
    (update-in [:chickn.core/operators] conj mutation-op)))


(defn run [cfg]
  (let [genotype (init cfg)]
    (evolve cfg genotype 100)))

(comment
  (run cfg-ones))

(comment
  (run cfg-zeros))
