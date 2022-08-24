(ns chickn.examples.tsp
  (:require [chickn.core :as chickn]
            [chickn.events :refer [monitor]]
            [chickn.util :refer [noop simple-printer]]
            [chickn.math :refer [rnd-index]]))

(def cities [{:name :A :x 1 :y 1}
             {:name :B :x 5 :y 1}
             {:name :C :x 10 :y 1}
             {:name :D :x 10 :y 5}
             {:name :E :x 10 :y 10}
             {:name :F :x 5 :y 10}
             {:name :G :x 1 :y 10}
             {:name :H :x 1 :y 5}])

(defn dist-squared [[{x1 :x y1 :y} {x2 :x y2 :y}]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(comment
  (dist-squared (first cities) (second cities)))

(defn fitness [cities]
  (apply + (map dist-squared (partition 2 1 cities))))

;; min distance 148

(defn init-pop [n]
  (repeatedly n #(shuffle cities)))

(defn init-pop2 [n cities-cnt]
  (repeatedly n
              #(for [i (range cities-cnt)]
                {:id (keyword (str "c" i)) :x (rand-int 100) :y (rand-int 100)})))

#_(println (init-pop 30))

#_(chickn/raw-pop->pop (init-pop 30))

#_(rnd-index [0 0 0 0 0 0 0])


(comment
  (let [cfg #:chickn.core{:chromo-gen  #(shuffle cities)
                          :pop-size    30
                          :terminated? noop
                          ;:monitor     monitor
                          :monitor     noop
                          :fitness     fitness
                          :comparator  chickn/lower-is-better
                          :reporter    simple-printer
                          :selector    #:chickn.selectors{:type        :chickn.selectors/roulette
                                                          :elit        true
                                                          :rate        0.3
                                                          :random-func rand}
                          :crossover   #:chickn.operators{:type         :chickn.operators/ordered-crossover
                                                          :rate         0.3
                                                          :random-point rnd-index
                                                          :rand-nth     rand-nth}
                          :mutation    #:chickn.operators{:type        :chickn.operators/swap-mutation
                                                          :rate        0.01
                                                          :rand-nth    rnd-index
                                                          :random-func rand}
                          :reinsertion #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                                            :rate 0.1}}
        genotype (chickn/init cfg)]
    (select-keys (chickn/evolve cfg genotype 100) [:iteration :time :best-chromo])))
