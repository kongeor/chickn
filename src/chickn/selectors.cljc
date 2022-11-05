(ns chickn.selectors
  (:require [clojure.spec.alpha :as s]
            [chickn.math :refer [rnd-index]]))

; -------
; Spec

(s/def ::rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def ::random-func ifn?)
(s/def ::duplicates? boolean?)
(s/def ::duplicate-checks int?)                             ;; TODO

(defmulti selector-type ::type)

(defmethod selector-type ::roulette [_]
  (s/keys :req [::type ::random-func ::rate]))

(defmethod selector-type ::best [_]
  (s/keys :req [::type ::rate]))

(defmethod selector-type ::tournament [_]
  (s/keys :req [::type ::random-func ::tour-size ::rate ::duplicates? ::duplicate-checks]))

(s/def ::selector (s/multi-spec selector-type ::type))

;; -----
;; util

(defn- identical-diff [xs ys]
  (remove (fn [e]
            (seq (filter #(identical? % e) ys))) xs))

; -------
; Natural Selectors

(defn -roulette
  [{comparator :chickn.core/comparator} {:keys [::random-func]} pop]                             ;; FIXME rename
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
              (recur w (inc i))))))))

(defn- -tour
  [{:keys [chickn.core/comparator]} {:keys [::random-func ::tour-size] :or {random-func rand}} pop]
  (let [f #(rnd-index random-func pop)
        idxs (repeatedly tour-size f)
        selected (map #(nth pop %) idxs)
        pop' (sort-by :fitness comparator selected)]
    (first pop')))

(defn roulette
  [{:keys [:chickn.selectors/rate] :as selector-cfg}]
  (fn [{:keys [:chickn.core/pop-size] :as cfg} chromos]
    (let [n        (int (* pop-size rate))
          roulette-f (partial -roulette cfg selector-cfg chromos)
          parents (repeatedly n roulette-f)
          leftover (identical-diff chromos parents)]
      {:parents  parents
       :leftover leftover})))

(defn best
  [_]
  (fn [_ chromos n]
    (take n chromos)))

(comment
  (let [xs [{:a 1} {:a 2} {:a 3}]]
    (identical-diff xs (take 2 xs))))

(defn- select-with-dup-check [n dup-checks selector-f]
  (loop [selected #{}
         i 0]
    (if (or
          (= (count selected) n)
          (>= i dup-checks))
      (vec selected)
      (recur
        (conj selected (selector-f))
        (inc i)))))

(defn tournament [{:keys [::rate ::duplicates? ::duplicate-checks] :or {duplicates? true duplicate-checks 10000} :as selector-cfg}]
  (fn [{:keys [:chickn.core/pop-size] :as cfg} chromos]
    (let [n        (int (* pop-size rate))
          tour-f   (partial -tour cfg selector-cfg chromos)
          parents  (if duplicates?
                     (repeatedly n tour-f)
                     (select-with-dup-check n duplicate-checks tour-f))
          leftover (identical-diff chromos parents)]
      {:parents  parents
       :leftover leftover})))

;; constructor funcs

(defmulti ->selector ::type)

(defmethod ->selector ::roulette [cfg]
  (roulette cfg))

(defmethod ->selector ::best [cfg]
  (best cfg))

(defmethod ->selector ::tournament [cfg]
  (tournament cfg))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 2 3] :fitness 2}
             {:genes [8 9 2 3] :fitness 4}
             {:genes [12 13 2 3] :fitness 8}]
        ; pop {:pop pop :total-fitness 15}
        random-func rand #_(constantly 0.5)]
    (with-redefs [shuffle identity]
      ((->selector {::type ::roulette ::random-func random-func}) {:chickn.core/comparator chickn.core/higher-is-better} pop 2))))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 2 3] :fitness 2}
             {:genes [8 9 2 3] :fitness 4}
             {:genes [12 13 2 3] :fitness 8}]
        random-func (chickn.util/val-cycle 0.0 0.25 0.5 0.25 0.5 0.75)]
    (with-redefs [shuffle identity]
      ((->selector {::type ::tournament ::random-func random-func ::tour-size 3 ::rate 0.75 ::duplicates? false}) {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/pop-size 4} pop))))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 2 3] :fitness 2}
             {:genes [8 9 2 3] :fitness 4}
             {:genes [12 13 2 3] :fitness 8}]
        random-func (chickn.util/val-cycle 0.0 0.25 0.5 0.25 0.5 0.75 0.8)]
    ((->selector {::type ::tournament ::random-func random-func
                  ::tour-size 3 ::rate 0.75 ::duplicates? false}) {:chickn.core/comparator chickn.core/lower-is-better :chickn.core/pop-size 4} pop)))
