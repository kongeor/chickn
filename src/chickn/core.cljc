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
   (let [ps (set (repeatedly ps #(rf (count c1))))]
     (loop [nc1 []
            nc2 []
            i 0
            c1 c1
            c2 c2]
       (let [[c1 c2] (if (some ps [i]) [c2 c1] [c1 c2])]
         (if (seq c1)
           (recur (conj nc1 (first c1)) (conj nc2 (first c2)) (inc i) (next c1) (next c2))
           [nc1 nc2]))))))

(defn mutate
  "For each gene of chromo c if mutation-rate is above
  the result of random function rf apply the function mf"
  [{:keys [mutation-rate rf mf]} c]
  (map #(if (> mutation-rate (rf)) (mf) %) c))

(defn roulette
  "Operates on a shuffled population"
  [{:keys [rf]} {pop :pop}]
  (let [total-fitness (->> pop (map :fitness) (apply +))
        roulette-pos (* (rf) total-fitness)
        pop-cnt (count pop)
        pop (shuffle pop)]
    (loop [w 0
           i 0]
      (if (>= i (dec pop-cnt))
        (first pop)
        (let [c (nth pop i)
              w (+ w (:fitness c))]
          (if (> w roulette-pos)
            c
            (recur w (inc i))))))))

(defn breed-pop [{:keys [rf crossover-rate crossover pop-size elitism-rate] :as cfg} pop]
  (map-indexed
    (fn [i {parent :genes}]
      (if (and (> crossover-rate (rf)) (>= (/ i pop-size) elitism-rate))
        (let [other (:genes (roulette cfg pop))]
          (first (crossover parent other)))
        parent)) (:pop pop)))

(defn raw-pop->pop [pop]
  {:pop (map (fn [c] {:genes c}) pop)})

(defn evolve [cfg pop]
  (raw-pop->pop (map (partial mutate cfg) (breed-pop cfg pop))))

(defn evolven [{:keys [terminated?] :as cfg} pop n]
  (let [cfg (assoc cfg :pop-size (count pop))]
    (loop [i 0
           pop pop]
      (let [pop (eval-pop cfg pop)
            best (:best-chromo pop)]
        (println "Iteration:" i "Fitness:" (:best-fitness pop) "Best Chromosome:" best )
        (if (or (>= i n) (terminated? best))
          best
          (recur (inc i) (evolve cfg pop)))))))

(defn gen-pop [pop-size chromo-size rf]
  (partition chromo-size (repeatedly (* pop-size chromo-size) rf)))

(comment
  (let [one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))
        pop (raw-pop->pop (gen-pop 300 28 one-or-zero))
        terminated? (fn [c] (= 28 (apply + c)))
        cfg {:terminated? terminated?
             :crossover-rate 0.3
             :mutation-rate 0.0001
             :elitism-rate 0.2
             :crossover (partial crossover rand-int 1)
             :mf one-or-zero
             :fitness (fn [c] (apply + c))
             :rf rand}
        cfg (assoc cfg :pop-size 300)
        g1 (eval-pop cfg pop)
        _ (clojure.pprint/pprint g1)
        g2 (evolve cfg g1)
        _ (evolven cfg pop 30000)
        ]
    #_(eval-pop cfg g2)))

(comment
  (crossover rand-int 1 [0 0 1 2] [0 0 1 2]))

