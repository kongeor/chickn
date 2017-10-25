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
  (fn [{:keys [total-fitness pop]} {:keys [chickn.core/pop-size]}]
    (let [roulette-pos (* (random-func) total-fitness)
          pop (shuffle pop)]
      (loop [w 0
             i 0]
        (if (> i pop-size)
          (last pop)
          (let [c (nth pop i)
                w (+ w (:fitness c))]
            (if (>= w roulette-pos)
              c
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