(ns chickn.operators
  (:require [clojure.spec.alpha :as s]
            [chickn.selectors :refer [->selector]]))

; -----
; Spec

(s/def ::rate-num (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def ::type keyword?)
(s/def ::rate ::rate-num)
(s/def ::elitism ::rate-num)
(s/def ::pointcuts integer?)
(s/def ::random-point ifn?)                                 ;; a func accepting a chromosome return a random position
(s/def ::random-func ifn?)                                  ;; duplicate with selectors?
(s/def ::mutation-func ifn?)

(s/def ::selector ifn?)                                     ; FIXME should live in it's own namespace

(defmulti operator-type ::type)

(defmethod operator-type ::order-crossover [_]
  (s/keys :req [::type ::rate ::elitism ::selector ::random-point]))

(defmethod operator-type ::cut-crossover [_]
  (s/keys :req [::type ::rate ::pointcuts ::elitism ::selector ::random-point]))

(defmethod operator-type ::rand-mutation [_]
  (s/keys :req [::type ::rate ::random-func ::elitism ::mutation-func]))

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

; ----
; constructor funcs

(defn crossover-pop [{:keys [::selector ::random-func ::rate ::elitism ::crossover]}]
  (let [selector (->selector selector)]
    (fn [pop]
      (let [pop-size (count pop)
            new-gen (map-indexed
                      (fn [i parent]
                        (if (and (> rate (random-func)) (>= (/ i pop-size) elitism))
                          (let [other (selector pop)]
                            (first (crossover parent other))) ;; TODO first only?
                          parent)) pop)]
        (into [] new-gen)))))


(defmulti operator ::type)

(defmethod operator ::cut-crossover [cfg]
  (crossover-pop (assoc cfg ::crossover (cut-crossover cfg))))

;; order-crossover

(defmethod operator ::rand-mutation [{:keys [::random-func ::rate ::elitism ::mutation-func]}]
  (fn [pop]
    (let [pop-size (count pop)]
      (map-indexed
        (fn [i c]
          (if (>= (/ i pop-size) elitism)
            (assoc-in c [:genes] (mapv #(if (> rate (random-func))
                                         (mutation-func)
                                         %) (:genes c)))
            c)) pop))))
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

#_(s/conform :crossover/crossover
           {:crossover/type :crossover/cut-crossover
            :crossover/rate 0.3
            :crossover/pointcuts 1})

#_(s/conform :crossover/crossover
           {:crossover/type :crossover/order-crossover
            :crossover/rate 0.3})
