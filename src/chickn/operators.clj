(ns chickn.operators
  (:require [clojure.spec :as s]))

; -----
; Spec

(s/def ::rate-num (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def :crossover/type keyword?)
(s/def :crossover/rate ::rate-num)
(s/def :crossover/pointcuts integer?)

(defmulti crossover-type :crossover/type)
(defmethod crossover-type :crossover/order-crossover [_]
  (s/keys :req [:crossover/type :crossover/rate]))

(defmethod crossover-type :crossover/cut-crossover [_]
  (s/keys :req [:crossover/type :crossover/rate :crossover/pointcuts]))

(s/def :crossover/crossover (s/multi-spec crossover-type :crossover/type))

; -----
; Genetic Operators

(defn cut-crossover [random-func]
  (fn [c1 c2]
    (let [i  (random-func c1)
          o1 (into [] (concat (take i c1) (drop i c2)))
          o2 (into [] (concat (take i c2) (drop i c1)))]
      [o1 o2])))


; ------
; Playground

(s/conform :crossover/crossover
           {:crossover/type :crossover/cut-crossover
            :crossover/rate 0.3
            :crossover/pointcuts 1})

(s/conform :crossover/crossover
           {:crossover/type :crossover/order-crossover
            :crossover/rate 0.3})
