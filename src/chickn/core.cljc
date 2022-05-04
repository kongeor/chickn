(ns chickn.core
  (:require [clojure.spec.alpha :as s]
            [chickn.operators :refer [->operator]]
            [chickn.selectors :refer [->selector]]
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
  {::pop-size 30
   ::monitor util/noop
   ::comparator  lower-is-better
   ::reporter    util/simple-printer
   ::selectors   [#:chickn.selectors{:type :chickn.selectors/best
                                     :elit true
                                     :rate 0.1
                                     :random-func rand}
                  #:chickn.selectors{:type        :chickn.selectors/roulette
                                     :rate        0.3
                                     :random-func rand}]
   ::operators   [#:chickn.operators{:type      :chickn.operators/cut-crossover
                                     :rate      0.3
                                     :pointcuts 1
                                     :rand-nth     rand-nth
                                     :random-point rand-nth
                                     :random-func  rand}]})

;; Spec

(s/def ::chromo-gen ifn?)
(s/def ::pop-size int?)
(s/def ::terminated? ifn?)
(s/def ::monitor ifn?)
(s/def ::fitness ifn?)
(s/def ::comparator ifn?)
(s/def ::reporter ifn?)
(s/def ::parallel boolean?)

(s/def ::selectors (s/+ :chickn.selectors/selector))
(s/def ::operators (s/+ :chickn.operators/operator))

(s/def ::config (s/keys :req [::chromo-gen ::pop-size ::terminated? ::monitor
                              ::fitness ::comparator ::reporter ::selectors
                              ::operators ::parallel]))


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


(defn apply-sels
  "Applies the provided configurations on the genotype
   and returns the vector of selected chromosomes"
  [{:keys [::pop-size ::monitor] :as cfg} sels-cfgs {pop :pop}]
  (apply concat
         (map (fn [sel-cfg]
                (let [sel-f (->selector sel-cfg)
                      rate (:chickn.selectors/rate sel-cfg)
                      n (Math/ceil (* pop-size rate))
                      chromos (sel-f cfg pop n)
                      _ (monitor :chickn.events/applied-selector "Selector applied" {:selector sel-cfg
                                                                                     :chromos chromos})]
                  chromos)) sels-cfgs)))

(defn mark-elits [chromos]
  (mapv #(assoc % :elit true) chromos))

(defn apply-ops [{:keys [::monitor] :as cfg} ops-cfgs chromos]
  (apply concat
         (map (fn [op-cfg]
                (let [op-f (->operator op-cfg)
                      rate (:chickn.operators/rate op-cfg)
                      n (Math/ceil (* (count chromos) rate))
                      operated (op-f cfg chromos n)
                      _ (monitor :chickn.events/operator-applied "Operator applied" {:operator op-cfg
                                                                                     :in-chromos chromos
                                                                                     :out-chromos operated})]
                  operated)) ops-cfgs)))

(defn adjust-size [{pop-size ::pop-size chromo-gen ::chromo-gen monitor ::monitor} chromos]
  (let [n (count chromos)]
    (monitor :chickn.events/adjusting-size (str "Adjusting chromos size: " n " to pop size: " pop-size) {})
    (cond
      (> n pop-size) (take n chromos)
      (< n pop-size) (let [adjusted (concat (map genes->chromo
                                                 (repeatedly (- pop-size n) chromo-gen)) chromos)]
                       (monitor :chickn.events/adjusted-size "Adjusted size" {:chromos adjusted})
                       adjusted))))

(defn evolve
  ([{:keys [::selectors ::operators ::monitor] :as cfg} genotype]
    (let [elit-sels-cfgs (filter :chickn.selectors/elit selectors)
          _ (monitor :chickn.events/selected-elit-cfgs "Selected elit configs" {:cfgs elit-sels-cfgs})
          sels-cfgs (remove :chickn.selectors/elit selectors)
          _ (monitor :chickn.events/selected-cfgs "Selected selector configs" {:cfgs sels-cfgs})
          elit-chromos (mark-elits (apply-sels cfg elit-sels-cfgs genotype))
          _ (monitor :chickn.events/elits-marked "Marked elits" {:chromos elit-chromos})
          selected-chromos (apply-sels cfg sels-cfgs genotype)
          _ (monitor :chickn.events/chromos-selected "Selected chromos" {:chromos selected-chromos})
          elits-and-rest-chromos (concat elit-chromos selected-chromos)
          operated-chromos (apply-ops cfg operators elits-and-rest-chromos)
          no-elits (remove :elit operated-chromos)
          new-gen-chromos (concat elit-chromos no-elits)
          adjusted-chromos (adjust-size cfg new-gen-chromos)
          new-genotype (eval-pop cfg (assoc genotype :pop adjusted-chromos))]
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

