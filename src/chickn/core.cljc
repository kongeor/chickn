(ns chickn.core)

(defn- mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn- std-dev [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (- total 1))
      (Math/sqrt))))

(defn eval-pop [{f :fitness} pop]
  (let [pop (->> pop :pop (map :genes))
        pop (mapv (fn [i] {:fitness (f i)
                       :genes   i}) pop)
        pop (sort-by :fitness #(compare %2 %1) pop)
        pop-avg (mean (map :fitness pop))
        std-dev (std-dev (map :fitness pop))
        best-fitness (:fitness (first pop))
        best-chromo (:genes (first pop))]
    {:pop-avg pop-avg
     :std-dev std-dev
     :best-fitness best-fitness
     :best-chromo best-chromo
     :pop pop}))

(defn crossover
  ([c1 c2] (crossover rand-int 1 c1 c2))
  ([rf ps c1 c2]
   (let [ps (set (take ps (repeatedly #(rf c1))))]
     (loop [nc1 []
            nc2 []
            i 0
            c1 c1
            c2 c2]
       (let [[c1 c2] (if (some ps [i]) [c2 c1] [c1 c2])]
         (if (seq c1)
           (recur (conj nc1 (first c1)) (conj nc2 (first c2)) (inc i) (next c1) (next c2))
           [nc1 nc2]))))))

(defn roulette
  [{:keys [rf]} pop]
  (let [total-fitness (->> pop (map :fitness) (apply +))
        roulette-pos (* (rf) total-fitness)
        pop-cnt (count pop)]
    (loop [w 0
           i 0]
      (if (>= i (dec pop-cnt))
        (first pop)
        (let [c (nth pop i)
              w (+ w (:fitness c))]
          (if (> w roulette-pos)
            c
            (recur w (inc i))))))))

(comment
  (let [pop [{:genes [0 0 1 2]
              :fitness 3}
             {:genes [0 0 1 1]
              :fitness 2}
             {:genes [0 1 0 0]
              :fitness 1}]]
    (clojure.pprint/pprint (roulette {:rf rand} pop))))

(defn evolve [cfg pop]
  )

(defn square [x] (* x x))
