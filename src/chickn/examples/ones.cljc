(ns chickn.examples.ones
  (:require [chickn.core :refer [default-cfg init
                                 init-and-evolve higher-is-better lower-is-better]]))

(def one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0)))

(def chromo-len 20)

(def chromo-gen #(repeatedly chromo-len one-or-zero))

(comment
  (chromo-gen))

(def mutation-op
  #:chickn.mutation
  {:type          :chickn.mutation/rand-mutation
   :rate          0.3
   :random-func   rand
   :mutation-func one-or-zero})

(def cfg-ones-base
  #:chickn.core
  {:chromo-gen chromo-gen
   :solved? (fn [_ {:keys [best-chromo]}] (every? #(= 1 %) best-chromo))
   :fitness #(apply + %)
   :comparator higher-is-better})

(def cfg-zeros-base
  (merge cfg-ones-base
    #:chickn.core
    {:solved? (fn [_ {:keys [best-chromo]}] (every? #(= 0 %) best-chromo))
     :fitness #(apply + %)
     :comparator lower-is-better}))


(def cfg-ones
  (-> default-cfg
    (merge cfg-ones-base)
    (assoc :chickn.core/mutation mutation-op)))

(def cfg-zeros
  (-> default-cfg
    (merge cfg-zeros-base)
    (assoc :chickn.core/mutation mutation-op)))


(defn run [cfg]
  (init-and-evolve cfg 1000))

(comment
  (run cfg-ones))

(comment
  (run cfg-zeros))
