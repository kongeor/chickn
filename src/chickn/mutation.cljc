(ns chickn.mutation
  (:require [clojure.spec.alpha :as s]
            [chickn.chromosome :as c]))

; -----
; Spec

(s/def ::rate-num (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def ::type keyword?)
(s/def ::rate ::rate-num)
(s/def ::rand-nth ifn?)
(s/def ::random-func ifn?)                                  ;; duplicate with selectors?
(s/def ::mutation-func ifn?)

(defmulti operator-type ::type)

(defmethod operator-type ::rand-mutation [_]
  (s/keys :req [::type ::rate ::random-func ::mutation-func]))

(defmethod operator-type ::swap-mutation [_]
  (s/keys :req [::type ::rate ::rand-nth ::random-func]))

(s/def ::mutation (s/multi-spec operator-type ::type))

; ----
; constructor funcs

(defmulti ->mutation ::type)

(defmethod ->mutation ::rand-mutation [{:keys [::random-func ::rate ::mutation-func]}]
  (fn [_ pop]
    (map
      (fn [c]
        (assoc-in c [:genes]
                  (mapv #(if (> rate (random-func))
                           (mutation-func)
                           %) (:genes c)))) pop)))

(defn swap-mutate [random-func chromo]
  (let [genes (:genes chromo)]
    (let [[p1 p2] (repeatedly 2 #(random-func genes))
          v1 (get genes p1)
          v2 (get genes p2)]
      (c/genes->chromo (assoc (assoc genes p1 v2) p2 v1)))))

(defmethod ->mutation ::swap-mutation [{:keys [::rand-nth ::rate ::random-func]}]
  (fn [_ pop]
    (mapv
      (fn [c]
        (if (> rate (random-func))
          (swap-mutate rand-nth c)
          c)) pop)))


(comment
  (let [pop (:pop (chickn.core/raw-pop->pop (partition 4 (range 16))))
        one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))]
    ((->mutation {::type          ::rand-mutation
                  ::rate          0.9
                  ::random-func   rand
                  ::mutation-func one-or-zero}) nil pop)))