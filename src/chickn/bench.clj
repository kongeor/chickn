(ns chickn.bench
  (:require [clojure.pprint :as pp]
            [chickn.core :refer [init-and-evolve]]
            [chickn.math :refer [cartesian-product mean]]
            [chickn.util :refer [noop]]
            [chickn.examples.ones :refer [cfg-ones]]))

(defn overr->vec
  "Given a override map returnes pairs

   For example:

  (overr->vec  {[:c1] [0 0.5 1] [:c2] [2 4 6]}) =>

  [[[[:c1] 0] [[:c1] 0.5] [[:c1] 1]] [[[:c2] 2] [[:c2] 4] [[:c2] 6]]]
  "
  [overr]
  (into [] (map (fn [[k vs]] (mapv (fn [v] [k v]) vs)) overr)))


(defn make-overr-product [overr]
  (->>
    (apply cartesian-product (overr->vec overr))
    (mapv #(apply concat %))
    (mapv #(apply hash-map %))))

(defn apply-overrides [cfg overr]
  (reduce (fn [acc op] (apply assoc-in acc op)) cfg overr))

(defn make-all-overrides [cfg overrides]
  (mapv #(apply-overrides cfg %) (make-overr-product overrides)))

;; FIXME make :time configurable, allow aliasing and show fitness average
(defn experiment [cfg overrides iters n]
  (let [cfgs (make-all-overrides cfg overrides)]
    (for [cfg' cfgs]
      (let [res (repeatedly n #(init-and-evolve cfg' iters))
            time (float (mean (map :time res)))
            fitness (float (mean (map #(get-in % [:genotype :best-fitness]) res)))]
        (assoc cfg' :time time :fitness fitness)))))

(defn get-override-cols [overrides names]
  (->> (vec (keys overrides))
       (map #(get names %))
       (remove nil?)
       (into [])))


(defn pp-experiment [cfg overrides iters n names]
  (let [res (experiment cfg overrides iters n)
  ;      _ (pp/pprint (first res))
        cols (conj (vec (keys overrides)) [:time] [:fitness])
        cols' (get-override-cols overrides names)
        cols' (conj cols' "time" "fitness") ;; FIXME
        res' (map (fn [r] (reduce #(assoc % (get names %2) (get-in r %2)) {}  cols)) res)]
    (pp/print-table cols' res')))

(def overrides
  {[:chickn.core/reporter] [noop]
   [:chickn.core/pop-size] [10 30 100]
   [:chickn.core/operators 0 :chickn.operators/rate] [0.1 0.5 0.9]
   [:chickn.core/operators 1 :chickn.operators/rate] [0.1 0.5 0.9]})

(def names
  {[:chickn.core/pop-size] "pop size"
   [:chickn.core/operators 0 :chickn.operators/rate] "crossover rate"
   [:chickn.core/operators 1 :chickn.operators/rate] "mutation rate"
   [:time] "time"
   [:fitness] "fitness"})

#_(pp-experiment cfg-ones overrides 10 10 names)

#_(def overrides {[:c1] [0 0.5 1]}
                [:c2] [2 4 6])


#_(clojure.pprint/pprint (overr->vec overrides))

#_(clojure.pprint/pprint (make-overr-product overrides))
#_(clojure.pprint/pprint (make-cfgs overrides))
#_(clojure.pprint/pprint (overr->vec overrides))
#_(clojure.pprint/pprint (apply-overrides cfg-ones (first (make-overr-product overrides))))

#_(pp/print-table)
#_(pp/pprint (experiment cfg-ones overrides 10 10))

#_(init-and-evolve (first (make-all-overrides cfg-ones overrides)) 10)
