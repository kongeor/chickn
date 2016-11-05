(ns chickn.operators
  (:require [clojure.spec :as s]))

(defn noop [& args])

; -----
; Spec

(s/def ::rate-num (s/double-in :min 0 :max 1 :NaN false :infinite? false))

(s/def ::type keyword?)
(s/def ::rate ::rate-num)
(s/def ::elitism ::rate-num)
(s/def ::pointcuts integer?)
(s/def ::random-point ifn?)                                 ;; a func accepting a chromosome return a random position

(s/def ::selector ifn?)                                     ; FIXME should live in it's own namespace

(defmulti operator-type ::type)

(defmethod operator-type ::order-crossover [_]
  (s/keys :req [::type ::rate ::elitism ::selector ::random-point]))

(defmethod operator-type ::cut-crossover [_]
  (s/keys :req [::type ::rate ::pointcuts ::elitism ::selector ::random-point]))

(s/def ::operator (s/multi-spec operator-type ::type))

; -----
; Genetic Operators

(defn cut-crossover [random-func]
  (fn [c1 c2]
    (let [i  (random-func c1)
          o1 (into [] (concat (take i c1) (drop i c2)))
          o2 (into [] (concat (take i c2) (drop i c1)))]
      [o1 o2])))


; ----
; constructor funcs

;; FIXME: remove
(defn make-selector [cfg])

(defn crossover-pop [{:keys [::selector ::random-func ::rate ::elitism ::crossover]}]
  (let [selector (make-selector selector)]
    (fn [pop]
      (let [pop-size (-> pop :pop count)
            new-gen (map-indexed
                      (fn [i parent]
                        (if (and (> rate (random-func)) (>= (/ i pop-size) elitism))
                          (let [other (selector pop)]
                            (first (crossover parent other)))
                          parent)) (:pop pop))]
        (assoc-in pop [:pop] new-gen)))))


(defmulti operator ::type)
(defmethod operator ::cut-crossover [cfg]
  (crossover-pop cfg))

(operator {::type ::cut-crossover
           ::rate 0.3
           ::elitism 0.1
           ::pointcuts 1
           ::selector noop})



#_(defn breed-pop [{:keys [random-func crossover-rate crossover elitism-rate] :as cfg} pop]
  (let [pop-size (-> pop :pop count)
        new-gen (map-indexed
                  (fn [i parent]
                    (if (and (> crossover-rate (random-func)) (>= (/ i pop-size) elitism-rate))
                      (let [other (roulette cfg pop)]
                        (first (crossover parent other)))
                      parent)) (:pop pop))]
    (assoc-in pop [:pop] new-gen)))

; ------
; Playground

#_(s/conform :crossover/crossover
           {:crossover/type :crossover/cut-crossover
            :crossover/rate 0.3
            :crossover/pointcuts 1})

#_(s/conform :crossover/crossover
           {:crossover/type :crossover/order-crossover
            :crossover/rate 0.3})
