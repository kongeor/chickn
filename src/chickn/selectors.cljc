(ns chickn.selectors
  (:require [clojure.spec.alpha :as s]))

; -------
; Spec

(s/def ::random-func ifn?)

(defmulti selector-type ::type)

(defmethod selector-type ::roulette [_]
  (s/keys :req [::type ::random-func]))

(s/def ::selector (s/multi-spec selector-type ::type))


; -------
; Natural Selectors

(defn roulette
  [{:keys [::random-func]}]
  (fn [{:keys [pop]} {:keys [chickn.core/comparator]}]
    (let [pop-size (count pop)
          max-fit (:fitness (first (sort-by :fitness #(compare %2 %1) pop)))
          pop (shuffle pop)
          fits-scaled (mapv #(/ (:fitness %) max-fit) pop)
          ascending? (= -1 (comparator 1 2))
          fits-scaled (if ascending? (mapv #(- 1.0 %) fits-scaled) fits-scaled)
          total-fitness (reduce + fits-scaled)
          roulette-pos (* (random-func) total-fitness)]
      (loop [w 0
             i 0]
        (if (> i pop-size)
          (last pop)
          (let [w (+ w (nth fits-scaled i))]
            (if (>= w roulette-pos)
              (nth pop i)
              (recur w (inc i)))))))))

;; constructor funcs

(defmulti ->selector ::type)

(defmethod ->selector ::roulette [cfg]
  (roulette cfg))


(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 2 3] :fitness 2}
             {:genes [8 9 2 3] :fitness 4}
             {:genes [12 13 2 3] :fitness 8}]
        pop {:pop pop :total-fitness 15}
        random-func rand #_(constantly 0.5)]
    (with-redefs [shuffle identity]
      ((->selector {::type ::roulette ::random-func random-func}) pop {:chickn.core/pop-size 4}))))