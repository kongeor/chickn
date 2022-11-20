(ns chickn.core
  (:require [chickn.crossover :refer [->crossover]]
            [chickn.events :refer [monitor]]
            [chickn.math :as math]
            [chickn.mutation :refer [->mutation]]
            [chickn.reinsertion :refer [->reinsertion]]
            [chickn.selector :refer [->selector]]
            [chickn.util :as util]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pp]))

(defn eval-pop [{fitness ::fitness comparator ::comparator monitor ::monitor parallel ::parallel} pop]
  (monitor :chickn.event/evaluating-population "Evaluating population" {:pop pop})
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

(def higher-is-better #(compare %2 %1))
(def lower-is-better compare)

(def default-cfg
  {::pop-size   30
   ::monitor    util/noop
   ::comparator lower-is-better
   ::reporter   util/simple-printer
   ::solved?    (constantly false)
   ::selector   #:chickn.selector{:type         :chickn.selector/roulette
                                   :rate        0.3
                                   :random-func rand}
   ::crossover  #:chickn.crossover{:type         :chickn.crossover/cut-crossover
                                   :rate         0.3
                                   :pointcuts    1
                                   :rand-nth     rand-nth
                                   :random-point rand-nth
                                   :random-func  rand}
   ::mutation   #:chickn.mutation{:type          :chickn.mutation/rand-mutation
                                   :rate          0.3
                                   :mutation-func rand
                                   :random-func   rand}
   ::reinsertion #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                      :rate 0.1}})

;; Spec

(s/def ::chromo-gen ifn?)
(s/def ::pop-size int?)
(s/def ::solved? ifn?)
(s/def ::monitor ifn?)
(s/def ::fitness ifn?)
(s/def ::comparator ifn?)
(s/def ::reporter ifn?)
(s/def ::parallel boolean?)

(s/def ::selector :chickn.selector/selector)
(s/def ::crossover :chickn.crossover/crossover)
(s/def ::mutation :chickn.mutation/mutation)
(s/def ::reinsertion :chickn.reinsertion/reinsertion)

(s/def ::config (s/keys :req [::chromo-gen ::pop-size ::solved? ::monitor
                              ::fitness ::comparator ::selector ::reinsertion]
                        :opt [::crossover ::mutation ::parallel ::reporter]))

(comment
  (s/explain-data ::config (assoc default-cfg
                             ::chromo-gen rand
                             ::solved? (constantly false)
                             ::fitness (constantly -1))))


;; FIXME implement
;; FIXME implement monitor func for auditing and debugging
(defn init
  "For the given cfg initialize the genotype.
   Returns an initialized and evaluated genotype (i.e. first gen)"
  [{:keys [::chromo-gen ::pop-size] :as cfg}]
  (let [raw-pop (raw-pop->pop (repeatedly pop-size chromo-gen))
        pop (eval-pop cfg raw-pop)]
    pop))


(defn evolve
  ([{:keys [::selector ::crossover ::mutation ::reinsertion ::pop-size] :as cfg} pop]
   (let [;; TODO optional
         pop' (:pop pop)
         selector-f (->selector selector)
         {:keys [parents leftover]} (selector-f cfg pop')
         crossover-f (->crossover crossover)
         children (crossover-f cfg parents)
         mutation-f (->mutation mutation)
         mutants (mutation-f cfg pop')
         offspring (concat children mutants)
         reinsertion-f (->reinsertion reinsertion)
         new-pop (reinsertion-f cfg {:parents parents :offspring offspring :leftover leftover})
         new-pop' (take pop-size new-pop)                   ;; TODO config?
         new-pop (eval-pop cfg (assoc pop :pop new-pop'))] ;; TODO meh?
     [cfg new-pop]))
  ([{:keys [::solved? ::reporter] :as cfg :or {reporter util/noop}} pop n]
   (loop [cfg   cfg
          pop   pop]
     (let [{:keys [best-chromo time]} pop]
       (reporter pop)
       (cond
         (solved? cfg pop) {:solved? true :iteration (:iteration pop) :best-chromo best-chromo :time time :pop pop}
         (>= (:iteration pop) n) {:solved? false :iteration (:iteration pop) :best-chromo best-chromo :time time :pop pop}
         :else (let [[cfg' pop'] (evolve cfg pop)]
                 (recur cfg' pop')))))))

(defn init-and-evolve [cfg n]
  (if (s/valid? ::config cfg)
    (let [pop (init cfg)]
      (evolve cfg pop n))
    (do
      (println "Config is not valid")
      (pp/pprint (s/explain-data ::config cfg)))))

;--------------
; Playground

(comment
  (s/explain ::cfg (merge default-cfg {:crossover-rate 2.})))

(comment
  (all-ones-sample))

(comment
  (crossover rand-int 1 [0 0 1 2] [0 0 1 2]))

