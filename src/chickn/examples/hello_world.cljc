(ns chickn.examples.hello-world
  (:require [chickn.core :refer [default-cfg init-and-evolve higher-is-better]]
            [chickn.util :as util]))

(def one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0)))

(def population-size 20)

(def chromo-gen #(repeatedly population-size one-or-zero))

(defn fitness [xs]
  (apply + xs))

(defn solved? [_ {:keys [best-chromosome]}]
  (every? #(= 1 %) best-chromosome))

(def mutation-op
  #:chickn.mutation
          {:type          :chickn.mutation/rand-mutation
           :rate          0.3
           :random-func   rand
           :mutation-func one-or-zero})

(def config (merge
              default-cfg
              #:chickn.core
                      {:chromo-gen chromo-gen
                       :fitness    fitness
                       :solved?    solved?
                       :reporter   util/noop
                       :mutation   mutation-op
                       :comparator higher-is-better}))

(comment
  (dissoc
    (init-and-evolve config 100) :population))