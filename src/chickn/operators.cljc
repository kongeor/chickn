(ns chickn.operators
  (:require [clojure.spec.alpha :as s]
            [chickn.selectors :refer [->selector]]))

; -----
; Spec

(s/def ::rate-num (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def ::type keyword?)
(s/def ::rate ::rate-num)
(s/def ::pointcuts integer?)
(s/def ::random-point ifn?)                                 ;; a func accepting a chromosome return a random position
(s/def ::random-func ifn?)                                  ;; duplicate with selectors?
(s/def ::mutation-func ifn?)

(defmulti operator-type ::type)

(defmethod operator-type ::ordered-crossover [_]
  (s/keys :req [::type ::rate ::random-point ::rand-nth]))

(defmethod operator-type ::cut-crossover [_]
  (s/keys :req [::type ::rate ::pointcuts ::random-point ::rand-nth]))

(defmethod operator-type ::rand-mutation [_]
  (s/keys :req [::type ::rate ::random-func ::mutation-func]))

(s/def ::operator (s/multi-spec operator-type ::type))

; -----
; Util

(defn genes->chromo [genes]
  {:genes genes
   :fitness 0                                               ; FIXME
   :age 0})

; -----
; Genetic Operators

(defn cut-crossover [{:keys [::random-point]}]
  (fn [{g1 :genes} {g2 :genes}]
    (let [i  (random-point g1)
          o1 (->> (concat (take i g1) (drop i g2)) (into []) genes->chromo)
          o2 (->> (concat (take i g2) (drop i g1)) (into []) genes->chromo)]
      [o1 o2])))

;; order-crossover

(defn ordered-crossover [{:keys [::random-point]}]
  (fn [{g1 :genes} {g2 :genes}]
    (let [g1 (vec g1)
          g2 (vec g2)
          p1 (random-point g1)
          p2 (random-point g2)
          [p1 p2] (sort [p1 p2])
          cut (subvec g1 p1 p2)
          rp (concat (drop p2 g2) (take p1 g2) (subvec g2 p1 p2))]
      (->
        (loop [c []
               i 0
               g rp]
          (if (= i (count g1))
            c
            (if (and (>= i p1) (< i p2))
              (recur (conj c (nth g1 i)) (inc i) g)
              (if (some (set (concat c cut)) (take 1 g))
                (recur c i (rest g))
                (recur (conj c (first g)) (inc i) (rest g))))))
        genes->chromo))))

(defn swap-mutate [random-func chromo]
  (let [genes (:genes chromo)]
    (let [[p1 p2] (repeatedly 2 #(random-func genes))
          v1 (get genes p1)
          v2 (get genes p2)]
      (genes->chromo (assoc (assoc genes p1 v2) p2 v1)))))

; ----
; constructor funcs

(defmulti ->operator ::type)

(defmethod ->operator ::cut-crossover [{:keys [::rand-nth ::rate] :as cfg}]
  (let [cross (cut-crossover cfg)]
    (fn [{:keys [:chickn.core/pop-size] :as cfg} pop]
      (let [n (int (* pop-size rate))]
        (->>
          (let [pairs (/ (if (= (mod n 2) 0) n (inc n)) 2)]
            (take
              pairs
              (repeatedly
                #(cross (rand-nth pop) (rand-nth pop)))))
          (apply concat)
          (take n)
          (into []))))))

(defmethod ->operator ::ordered-crossover [{:keys [::rand-nth ::rate] :as cfg}]
  (let [cross (ordered-crossover cfg)]
    (fn [{:keys [:chickn.core/pop-size] :as cfg} chromos]
      (let [n (int (* pop-size rate))]
        (->>
          (repeatedly
            #(cross (rand-nth chromos) (rand-nth chromos)))
          (take n)
          (into []))))))

(defmethod ->operator ::rand-mutation [{:keys [::random-func ::rate ::mutation-func]}]
  (fn [_ pop]
    (map
      (fn [c]
        (assoc-in c [:genes]
          (mapv #(if (> rate (random-func))
                   (mutation-func)
                   %) (:genes c)))) pop)))

(defmethod ->operator ::swap-mutation [{:keys [::rand-nth ::rate ::random-func]}]
  (fn [_ pop]
    (mapv
      (fn [c]
        (if (> rate (random-func))
          (swap-mutate rand-nth c)
          c)) pop)))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 6 7] :fitness 1}
             {:genes [8 9 10 11] :fitness 1}
             {:genes [12 13 14 15] :fitness 1}]
        random-func (constantly 0.5)]
    ((->operator {::type ::cut-crossover
                  ::rate 0.3
                  ::pointcuts 1
                  ::random-point rand-nth}) pop {:chickn.core/pop-size 10 :chickn.core/elitism-rate 0.56})))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 6 7] :fitness 1}
             {:genes [8 9 10 11] :fitness 1}
             {:genes [12 13 14 15] :fitness 1}]
        random-func (constantly 0.2)]
    ((->operator {::type ::rand-mutation
                  ::rate 0.3
                  ::random-func random-func
                  ::mutation-func (constantly -1)}) pop {})))

; ------
; Playground

(comment
  (let [pop (:pop (chickn.core/raw-pop->pop (partition 4 (range 16))))
        sel-cfg #:chickn.selectors{:type :chickn.selectors/roulette
                                                  :random-func rand}]
    ((->selector sel-cfg) pop)
    #_((operator {::type         ::cut-crossover
                ::rate         1.0
                ::elitism      0.0
                ::pointcuts    1
                ::random-point (fn [& _] 2)
                ::random-func  rand
                ::selector     sel-cfg}) pop)))

(comment
  (let [pop (:pop (chickn.core/raw-pop->pop (partition 4 (range 16)))) ;; TODO make test
        mut-cfg {::type ::rand-mutation
                 ::rate 0.3
                 ::random-func rand
                 ::elitism 0
                 ::mutation-func (constantly -1)}]
    ((operator mut-cfg) pop)))

(comment
  (s/conform ::operator
             {::type ::cut-crossover
              ::rate 0.3
              ::pointcuts 1
              ::random-point rand-nth}))
#_(s/conform :crossover/crossover
           {:crossover/type :crossover/order-crossover
            :crossover/rate 0.3})
