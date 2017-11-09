(ns chickn.core
  (:require [clojure.spec.alpha :as s]
            [chickn.operators :refer [->operator]]
            [chickn.selectors :refer [->selector]]
            [chickn.util :as util]
            [chickn.math :as math]))

(defn eval-pop [{fitness ::fitness comparator ::comparator} pop]
  (let [iteration ((fnil inc 0) (:iteration pop))
        pop (vec (map (fn [{:keys [genes age]}]
                        {:fitness (fitness genes)
                         :genes genes
                         :age ((fnil inc 0) age)}) (:pop pop)))
        pop (sort-by :fitness comparator pop)
        pop-avg (math/mean (map :fitness pop))
        std-dev (math/std-dev (map :fitness pop))
        age-avg (math/mean (map :age pop))
        best-fitness (:fitness (first pop))
        best-chromo (:genes (first pop))]
    {:pop-avg (float pop-avg)
     :age-avg (float age-avg)
     :std-dev std-dev
     :best-fitness best-fitness
     :best-chromo best-chromo
     :total-fitness (reduce + (map :fitness pop))
     :pop pop
     :iteration iteration}))

(defn genes->chromo [genes]
  {:genes (vec genes)
   :fitness 0                                               ; FIXME
   :age 0})

(defn raw-pop->pop [pop]
  {:pop (mapv genes->chromo pop)})

(defn gen-pop [pop-size chromo-size rf]
  (partition chromo-size (repeatedly (* pop-size chromo-size) rf)))


(def default-cfg {:crossover-rate 0.3
                  :mutation-rate  0.05
                  :elitism-rate   0.1
                  :random-func    rand
                  :reporter       util/simple-printer})
;--------------
; Playground

(s/def ::crossover-rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))
(s/def ::mutation-rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))
(s/def ::elitism-rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))
(s/def ::random-func ifn?)
(s/def ::reporter ifn?)

(s/def ::cfg (s/keys :req-un [::crossover-rate ::mutation-rate ::elitism-rate
                              ::random-func ::reporter]))

;; FIXME implement
;; FIXME rename top level pop -> genotype
;; FIXME implement monitor func for auditing and debugging
;; FIXME make cfg always the first param

(defn init
  "For the given cfg initialize the genotype.
   Returns a tuple of [cfg, genotype]"
  [cfg])

(defn evolve
  ([cfg pop])
  ([cfg pop n]))

(defn evolve* [{:keys [::chromo-gen ::pop-size ::terminated?
                       ::selector ::operators ::reporter ::elitism-rate] :as cfg} n]
  (let [pop (raw-pop->pop (repeatedly pop-size chromo-gen))
        selector (->selector selector)
        opts (map ->operator operators)
        ;evol (fn [pop] (assoc pop :pop (reduce #(%2 %1 cfg) (:pop pop) opts)))
        evol (fn [pop] (reduce #(%2 %1 cfg) pop opts))
        elit-cnt (* pop-size elitism-rate)
        mating-pop-size (- pop-size elit-cnt)
        start (util/now-millis)
        endf #(- (util/now-millis) start)]
    (loop [pop pop]
      (let [pop (eval-pop cfg pop)
            best (:best-chromo pop)]
        #_(println (selector pop cfg))
        (reporter pop)
        (cond
          (terminated? best) {:solved? true :time (endf) :best best}
          (>= (:iteration pop) n) {:solved? false :time (endf)}
          :else (recur (let [mating-pop (map (fn [_] (selector pop cfg)) (range mating-pop-size))
                             elit (take elit-cnt (:pop pop))
                             new-gen (evol mating-pop)
                             all-new (concat elit new-gen)]
                         (assoc pop :pop all-new))))))))

;; Spec

(s/def ::fitness ifn?)
(s/def ::init-pop ifn?)
(s/def ::terminated? ifn?)
(s/def ::operators (s/+ :chickn.operators/operator))

(s/def ::config (s/keys :req [::init-pop ::elitism-rate ::pop-size ::terminated? ::operators]
                        :req-un [::fitness]))

(def descending #(compare %2 %1))
(def ascending compare)

(comment
  (let [chromo-len 256
        one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))
        cfg {::chromo-gen #(repeatedly chromo-len one-or-zero)
             ::pop-size 30
             ::rand-nth rand-nth                            ;; FIXME this will be marked as obsolete
             ::elitism-rate 0.1                             ;; FIXME this will be substituted by threshold selector
             ::terminated? (fn [c] (every? #(= 1 %) c))
             ::fitness      (fn [c] (apply + c))
             ::comparator  descending                       ;; FIXME rename to better-fitness increasing/decreasing
             ::reporter    util/simple-printer
             ::selector    #:chickn.selectors{:type        :chickn.selectors/roulette ;; FIXME make vector
                                              :random-func rand} ;; FIXME should accept rate
             ::operators   [#:chickn.operators{:type         :chickn.operators/cut-crossover
                                               :rate         0.3
                                               :pointcuts    1
                                               :random-point rand-nth
                                               :random-func  rand}
                            #:chickn.operators{:type          :chickn.operators/rand-mutation
                                               :rate          0.001
                                               :random-func   rand
                                               :mutation-func one-or-zero}]}]
    (select-keys (evolve* cfg 10000) [:solved? :time])))

(comment
  (let [one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))
        cfg {::init-pop    #(raw-pop->pop (gen-pop 30 256 one-or-zero))
             ::pop-size 30                                  ;; Check line above
             ::rand-nth rand-nth
             ::elitism-rate 0.1
             ::terminated? (fn [c] (= 0 (apply + c)))
             :fitness      (fn [c] (apply + c))
             ::comparator  ascending
             ::reporter    simple-print
             ::selector    #:chickn.selectors{:type        :chickn.selectors/roulette
                                              :random-func rand}
             ::operators   [#:chickn.operators{:type         :chickn.operators/cut-crossover
                                               :rate         0.3
                                               :pointcuts    1
                                               :random-point rand-nth
                                               :random-func  rand}
                            #:chickn.operators{:type          :chickn.operators/rand-mutation
                                               :rate          0.01
                                               :random-func   rand
                                               :mutation-func one-or-zero}]}]
    (:solved? (evolve* cfg 3000))))

(comment
  (s/explain ::cfg (merge default-cfg {:crossover-rate 2.})))

(comment
  (all-ones-sample))

(comment
  (crossover rand-int 1 [0 0 1 2] [0 0 1 2]))

