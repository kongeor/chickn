(ns chickn.crossover
  (:require [clojure.spec.alpha :as s]
            [chickn.chromosome :as c]))

; -----
; Spec

(s/def ::rate-num (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def ::type keyword?)
(s/def ::rate ::rate-num)
(s/def ::pointcuts integer?)
(s/def ::random-point ifn?)                                 ;; a func accepting a chromosome return a random position
(s/def ::random-func ifn?)                                  ;; duplicate with selectors?
(s/def ::mutation-func ifn?)

(defmulti crossover-type ::type)

(defmethod  crossover-type ::ordered-crossover [_]
  (s/keys :req [::type ::rate ::random-point ::rand-nth]))

(defmethod crossover-type ::cut-crossover [_]
  (s/keys :req [::type ::rate ::pointcuts ::rand-nth]))

(s/def ::crossover (s/multi-spec crossover-type ::type))

; -----
; Impl

(defn cut-crossover [{:keys [::random-point]}]
  (fn [{g1 :genes} {g2 :genes}]
    (let [i  (random-point g1)
          o1 (->> (concat (take i g1) (drop i g2)) (into []) c/genes->chromo)
          o2 (->> (concat (take i g2) (drop i g1)) (into []) c/genes->chromo)]
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
        c/genes->chromo))))


; ----
; constructor funcs

(defmulti ->crossover ::type)

(defmethod ->crossover ::cut-crossover [{:keys [::rand-nth ::rate] :as cfg}]
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

(defmethod ->crossover ::ordered-crossover [{:keys [::rand-nth ::rate] :as cfg}]
  (let [cross (ordered-crossover cfg)]
    (fn [{:keys [:chickn.core/pop-size] :as cfg} chromos]
      (let [n (int (* pop-size rate))]
        (->>
          (repeatedly
            #(cross (rand-nth chromos) (rand-nth chromos)))
          (take n)
          (into []))))))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 6 7] :fitness 1}
             {:genes [8 9 10 11] :fitness 1}
             {:genes [12 13 14 15] :fitness 1}]
        random-func (constantly 0.5)]
    ((->crossover {::type ::cut-crossover
                  ::rate 0.3
                  ::pointcuts 1
                  ::random-point rand-nth}) pop {:chickn.core/pop-size 10 :chickn.core/elitism-rate 0.56})))

(comment
  (let [pop [{:genes [0 1 2 3] :fitness 1}
             {:genes [4 5 6 7] :fitness 1}
             {:genes [8 9 10 11] :fitness 1}
             {:genes [12 13 14 15] :fitness 1}]
        random-func (constantly 0.2)]
    ((->crossover {::type ::rand-mutation
                  ::rate 0.3
                  ::random-func random-func
                  ::mutation-func (constantly -1)}) pop {})))

; ------
; Playground

(comment
  (let [pop (:pop (chickn.core/raw-pop->pop (partition 4 (range 16)))) ;; TODO make test
        mut-cfg {::type ::rand-mutation
                 ::rate 0.3
                 ::random-func rand
                 ::elitism 0
                 ::mutation-func (constantly -1)}]
    ((crossover mut-cfg) pop)))

(comment
  (s/conform ::crossover
             {::type ::cut-crossover
              ::rate 0.3
              ::pointcuts 1
              ::random-point rand-nth}))
#_(s/conform :crossover/crossover
           {:crossover/type :crossover/order-crossover
            :crossover/rate 0.3})
