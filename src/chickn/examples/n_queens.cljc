(ns chickn.examples.n-queens
  (:require [clojure.string :as str]
            [chickn.core :as chickn]
            [chickn.util :as util]
            [chickn.math :as math]))

(def c0 [2 0 3 1])
(def c1 [0 0 3 1])
(def c2 [0 1 3 2])

(defn row-clashes [c]
  (- (count c) (count (set c))))

(defn diag-clashes [c]
  (let [n (count c)
        cl (for [i (range n)
                 j (range n)
                 :when (not= i j)]
             (let [dx (abs (- i j))
                   dy (abs (- (c i) (c j)))]
               (if (= dx dy)
                 1
                 0)))]
    (count (filter #(not= % 0) cl))))

(defn fitness [c]
  (+ (row-clashes c) (diag-clashes c)))

(comment
  (fitness c0))

(defn row-str [n i]
  (str/join " "
    (map (fn [idx]
           (if (= idx i)
             "Q"
             "_")) (range n))))

(defn print-board [c]
  (let [n (count c)]
    (doall
      (for [i (range n)]
        (println (row-str n (c i)))))
    nil))

(comment
  (print-board c2))


(defn printer [{:keys [iteration best-fitness best-chromo]}]
  (println "Iteration" iteration)
  (print-board best-chromo)
  (println)
 )

(defn queens [n]
  (let [cfg #:chickn.core{:chromo-gen  #(shuffle (range n))
                          :pop-size    20
                          :terminated? #(= (fitness %) 0)   ;; TODO need a better way , plus duplicated iteration
                          :monitor     util/noop
                          :fitness     fitness
                          :comparator  chickn/lower-is-better
                          :reporter    printer
                          :selector   #:chickn.selectors{:type        :chickn.selectors/tournament
                                                          :rate        0.3
                                                          :random-func rand
                                                          :tour-size   10}
                          :crossover   #:chickn.operators{:type         :chickn.operators/ordered-crossover
                                                          :rate         0.5
                                                          :random-point math/rnd-index
                                                          :rand-nth     rand-nth}
                          :mutation    #:chickn.operators{:type        :chickn.operators/swap-mutation
                                                          :rate        0.7
                                                          :rand-nth    math/rnd-index
                                                          :random-func rand}
                          :reinsertion #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                                            :rate 0.1}}
        genotype (chickn/init cfg)
        result (chickn/evolve cfg genotype 2000)]
    (if (:solved? result)
      (println "solved after" (:iteration result) "iterations")
      (println "not solved :("))
    (print-board (-> result :genotype :best-chromo))))

(comment
  (queens 4)
  (queens 8)
  (queens 16)
  (queens 32))
