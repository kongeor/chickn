(ns chickn.core
  (:require [clojure.spec :as s]))

; -------------
; Utilites

(defn val-cycle
  "Retuns a func that upon each invocation
  returns each passed element.

  Used for dev and testing. Will be (re)moved."
  [& vals]
  (let [c (atom (cycle vals))]
    (fn [& _]
      (let [f (first @c)
            n (next @c)]
        (reset! c n)
        f))))

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

(defn eval-pop [{fitness :fitness} pop]
  (let [iteration ((fnil inc 0) (:iteration pop))
        pop (mapv (fn [{:keys [genes age]}]
                    {:fitness (fitness genes)
                     :genes genes
                     :age ((fnil inc 0) age)}) (:pop pop))
        pop (sort-by :fitness #(compare %2 %1) pop)
        pop-avg (mean (map :fitness pop))
        std-dev (std-dev (map :fitness pop))
        age-avg (mean (map :age pop))
        best-fitness (:fitness (first pop))
        best-chromo (:genes (first pop))]
    {:pop-avg (float pop-avg)
     :age-avg (float age-avg)
     :std-dev std-dev
     :best-fitness best-fitness
     :best-chromo best-chromo
     :pop pop
     :iteration iteration}))

(defn crossover
  ([c1 c2] (crossover rand-int 1 c1 c2))
  ([random-func ps {c1 :genes} {c2 :genes}]
   (let [ps (set (repeatedly ps #(random-func (count c1))))]
     (loop [nc1 []
            nc2 []
            i 0
            c1 c1
            c2 c2]
       (let [[c1 c2] (if (some ps [i]) [c2 c1] [c1 c2])]
         (if (seq c1)
           (recur (conj nc1 (first c1)) (conj nc2 (first c2)) (inc i) (next c1) (next c2))
           [{:genes nc1 :age 0} {:genes nc2 :age 0}]))))))

(defn swap-mutate [random-func chromo]
  (let [[p1 p2] (repeatedly 2 #(random-func chromo))
        v1 (get chromo p1)
        v2 (get chromo p2)]
    (assoc (assoc chromo p1 v2) p2 v1)))

(s/def ::gene (s/int-in 0 2))

(s/def ::chromo (s/coll-of ::gene :kind vector? :count 4))

(s/fdef swap-mutate
  :args (s/cat :random-func (s/fspec :args ::chromo
                                     :ret (s/int-in 0 4))
               :chromo ::chromo)
  :ret ::chromo)

(defn mutate
  "For each gene of chromo c if mutation-rate is above
  the result of random function rf apply the function mf"
  [{:keys [mutation-rate random-func mutation-func elitism-rate]} pop]
  (let [pop-size (-> pop :pop count)
        mutants (map-indexed
                  (fn [i c]
                    (if (>= (/ i pop-size) elitism-rate)
                      (assoc-in c [:genes] (mapv #(if (> mutation-rate (random-func))
                                                   (mutation-func)
                                                   %) (:genes c)))
                      c)) (:pop pop))]
    (assoc-in pop [:pop] mutants)))

(defn roulette
  "Operates on a shuffled population"
  [{:keys [random-func]} {pop :pop}]
  (let [total-fitness (->> pop (map :fitness) (apply +))
        roulette-pos (* (random-func) total-fitness)
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

(defn breed-pop [{:keys [random-func crossover-rate crossover elitism-rate] :as cfg} pop]
  (let [pop-size (-> pop :pop count)
        new-gen (map-indexed
                  (fn [i parent]
                    (if (and (> crossover-rate (random-func)) (>= (/ i pop-size) elitism-rate))
                      (let [other (roulette cfg pop)]
                        (first (crossover parent other)))
                      parent)) (:pop pop))]
    (assoc-in pop [:pop] new-gen)))

(defn raw-pop->pop [pop]
  {:pop (map (fn [c] {:genes c}) pop)})

(defn evolve [cfg pop]
  (let [new-breed (breed-pop cfg pop)]
    (mutate cfg new-breed)))

(defn evolven [{:keys [terminated? reporter] :as cfg} pop n]
  (loop [pop pop]
    (let [pop (eval-pop cfg pop)
          best (:best-chromo pop)]
      (reporter pop)
      (cond
        (terminated? best) {:solved? true :best best}
        (>= (:iteration pop) n) false
        :else (recur (evolve cfg pop))))))

(defn gen-pop [pop-size chromo-size rf]
  (partition chromo-size (repeatedly (* pop-size chromo-size) rf)))

(defn fancy-print []
  (let [ind-tokens [\| \\ \- \/]
        sz (dec (count ind-tokens))
        i (atom 0)
        inc-tk #(let [p (if (= @i sz) (reset! i 0) (swap! i inc))]
                 (get ind-tokens p))]
    (fn [pop]
      (print (inc-tk) "Iter: " 0 "Fitness:" (:best-fitness pop) "Best:" (:best-chromo pop)))))

(defn simple-print [{:keys [iteration best-fitness best-chromo]}]
  (println "Iter:" iteration "Fitness:" best-fitness "Best:" best-chromo))


(def default-cfg {:crossover-rate 0.3
                  :mutation-rate  0.05
                  :elitism-rate   0.1
                  :random-func    rand
                  :reporter       simple-print})
;--------------
; Playground

(defn all-ones-sample []
  (let [one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))
        pop (raw-pop->pop (gen-pop 30 28 one-or-zero))
        terminated? (fn [c] (= 28 (apply + c)))
        cfg (merge
              default-cfg
              {:terminated?    terminated?
               :crossover      (partial crossover rand-int 1)
               :mutation-func  one-or-zero
               :fitness        (fn [c] (apply + c))})]
    (evolven cfg pop 30000)))

(comment
  (all-ones-sample))

(comment
  (crossover rand-int 1 [0 0 1 2] [0 0 1 2]))

