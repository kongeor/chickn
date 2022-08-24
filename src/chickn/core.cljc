(ns chickn.core
  (:require [clojure.spec.alpha :as s]
            [chickn.operators :refer [->operator]]
            [chickn.selectors :refer [->selector]]
            [chickn.reinsertion :refer [->reinsertion]]
            [chickn.events :refer [monitor]]
            [chickn.util :as util]
            [chickn.math :as math]))

(defn eval-pop [{fitness ::fitness comparator ::comparator monitor ::monitor parallel ::parallel} pop]
  (monitor :chickn.event/evaluating-genotype "Evaluating genotype" {:genotype pop})
  (let [start-time (util/now-millis)
        iteration ((fnil inc 0) (:iteration pop))
        pop (vec ((if parallel pmap map)
                  (fn [{:keys [genes age]}]
                    {:fitness (fitness genes)
                     :genes   genes
                     :age     ((fnil inc 0) age)}) (:pop pop)))
        pop (sort-by :fitness comparator pop)
        pop-avg (math/mean (map :fitness pop))
        std-dev (math/std-dev (map :fitness pop))
        age-avg (math/mean (map :age pop))
        best-fitness (:fitness (first pop))
        best-chromo (:genes (first pop))
        time (- (util/now-millis) start-time)]
    {:pop-avg (float pop-avg)
     :age-avg (float age-avg)
     :std-dev std-dev
     :best-fitness best-fitness
     :best-chromo best-chromo
     :total-fitness (reduce + (map :fitness pop))
     :pop pop
     :iteration iteration
     :time time}))

(defn genes->chromo [genes]
  {:genes (vec genes)
   :fitness 0                                               ; FIXME
   :age 0})

(defn raw-pop->pop [pop]
  {:pop (mapv genes->chromo pop)})

(defn gen-pop [pop-size chromo-size rf]
  (partition chromo-size (repeatedly (* pop-size chromo-size) rf)))


(def higher-is-better #(compare %2 %1))
(def lower-is-better compare)

(def default-cfg
  {::pop-size   30
   ::monitor    util/noop
   ::comparator lower-is-better
   ::reporter   util/simple-printer
   ::selector   #:chickn.selectors{:type        :chickn.selectors/roulette
                                   :rate        0.3
                                   :random-func rand}
   ::crossover  #:chickn.operators{:type         :chickn.operators/cut-crossover
                                   :rate         0.3
                                   :pointcuts    1
                                   :rand-nth     rand-nth
                                   :random-point rand-nth
                                   :random-func  rand}
   ::mutation   #:chickn.operators{:type         :chickn.operators/rand-mutation
                                   :rate         0.3
                                   :mutation-func rand
                                   :random-func   rand}
   ::reinsertion #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                      :rate 0.1}})

;; Spec

(s/def ::chromo-gen ifn?)
(s/def ::pop-size int?)
(s/def ::terminated? ifn?)
(s/def ::monitor ifn?)
(s/def ::fitness ifn?)
(s/def ::comparator ifn?)
(s/def ::reporter ifn?)
(s/def ::parallel boolean?)

(s/def ::selector (s/? :chickn.selectors/selector))
(s/def ::crossover (s/? :chickn.operators/operator))
(s/def ::mutation (s/? :chickn.operators/operator))
(s/def ::reinsertion (s/? :chickn.reinsertion/reinsertion))

(s/def ::config (s/keys :req [::chromo-gen ::pop-size ::terminated? ::monitor
                              ::fitness ::comparator ::reporter ::selector
                              ::crossover ::mutation ::reinsertion ::parallel]))


;; FIXME implement
;; FIXME rename top level pop -> genotype
;; FIXME implement monitor func for auditing and debugging
(defn init
  "For the given cfg initialize the genotype.
   Returns an initialized and evaluated genotype (i.e. first gen)"
  [{:keys [::chromo-gen ::pop-size] :as cfg}]
  (let [raw-genotype (raw-pop->pop (repeatedly pop-size chromo-gen))
        genotype (eval-pop cfg raw-genotype)]
    genotype))


(defn evolve
  ([{:keys [::selector ::crossover ::mutation ::reinsertion ::pop-size] :as cfg} genotype]
   (let [;; TODO optional
         pop (:pop genotype)
         selector-f (->selector selector)
         {:keys [parents leftover]} (selector-f cfg pop)
         crossover-f (->operator crossover)
         children (crossover-f cfg parents)
         mutation-f (->operator mutation)
         mutants (mutation-f cfg pop)
         offspring (concat children mutants)
         reinsertion-f (->reinsertion reinsertion)
         new-pop (reinsertion-f cfg {:parents parents :offspring offspring :leftover leftover})
         new-pop' (take pop-size new-pop)                   ;; TODO config?
         new-genotype (eval-pop cfg (assoc genotype :pop new-pop'))] ;; TODO meh?
     [cfg new-genotype]))
  ([{:keys [::terminated? ::reporter] :as cfg} genotype n]
   (loop [cfg      cfg
          genotype genotype]
     (let [best (:best-chromo genotype)]
       (reporter genotype)
       (cond
         (terminated? best) {:solved? true :iteration (:iteration genotype) :best best :genotype genotype}
         (>= (:iteration genotype) n) {:solved? false :iteration (:iteration genotype) :genotype genotype}
         :else (let [[cfg' genotype'] (evolve cfg genotype)]
                 (recur cfg' genotype')))))))

(defn init-and-evolve [cfg n]
  (let [genotype (init cfg)]
    (evolve cfg genotype n)))

;--------------
; Playground

(comment
  (s/explain ::cfg (merge default-cfg {:crossover-rate 2.})))

(comment
  (all-ones-sample))

(comment
  (crossover rand-int 1 [0 0 1 2] [0 0 1 2]))

