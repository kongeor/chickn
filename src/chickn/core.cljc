(ns chickn.core
  (:require [clojure.spec.alpha :as s]
            [chickn.operators :refer [operator ->operator]]
            [chickn.selectors :refer [->selector]]))

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

(defn eval-pop [{fitness :fitness comparator ::comparator} pop]
  (let [iteration ((fnil inc 0) (:iteration pop))
        pop (mapv (fn [{:keys [genes age]}]
                    {:fitness (fitness genes)
                     :genes genes
                     :age ((fnil inc 0) age)}) (:pop pop))
        pop (sort-by :fitness comparator pop)
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
     :total-fitness (reduce + (map :fitness pop))
     :pop pop
     :iteration iteration}))

(defn ordered-crossover [rf {g1 :genes} {g2 :genes}]
  (let [[p1 p2] (sort (repeatedly 2 rf))
        cut (subvec g1 p1 p2)
        rp (concat (drop p2 g2) (take p1 g2) (subvec g2 p1 p2))]
    (loop [c []
           i 0
           g rp]
      (if (= i (count g1))
        c
        (if (and (>= i p1) (< i p2))
          (recur (conj c (nth g1 i)) (inc i) g)
          (if (some (set (concat c cut)) (take 1 g))
            (recur c i (rest g))
            (recur (conj c (first g)) (inc i) (rest g))))))))

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

(defn genes->chromo [genes]
  {:genes genes
   :fitness 0                                               ; FIXME
   :age 0})

(defn raw-pop->pop [pop]
  {:pop (map genes->chromo pop)})

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

(s/def ::crossover-rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))
(s/def ::mutation-rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))
(s/def ::elitism-rate (s/double-in :min 0 :max 1 :NaN false :infinite? false))
(s/def ::random-func ifn?)
(s/def ::reporter ifn?)

(s/def ::cfg (s/keys :req-un [::crossover-rate ::mutation-rate ::elitism-rate
                              ::random-func ::reporter]))

(defn evolve* [{:keys [::init-pop ::pop-size ::terminated?
                       ::selector ::operators ::reporter ::elitism-rate] :as cfg} n]
  (let [pop (init-pop)
        selector (->selector selector)
        opts (map ->operator operators)
        ;evol (fn [pop] (assoc pop :pop (reduce #(%2 %1 cfg) (:pop pop) opts)))
        evol (fn [pop] (reduce #(%2 %1 cfg) pop opts))
        elit-cnt (* pop-size elitism-rate)
        mating-pop-size (- pop-size elit-cnt)]
    (loop [pop pop]
      (let [pop (eval-pop cfg pop)
            best (:best-chromo pop)]
        #_(println (selector pop cfg))
        (reporter pop)
        (cond
          (terminated? best) {:solved? true :best best}
          (>= (:iteration pop) n) false
          :else (recur (let [mating-pop (repeatedly mating-pop-size #(selector pop cfg))
                             elit (take elit-cnt (:pop pop))
                             new-gen (evol mating-pop)
                             all-new (concat elit new-gen)]
                         (assoc pop :pop all-new))))))))

;; Spec

(s/def ::fitness ifn?)
(s/def ::init-pop ifn?)
(s/def ::terminated? ifn?)
(s/def ::operators (s/+ :chickn.operators/operator))

(s/def ::config (s/keys :req [::init-pop ::elitism-rate ::pop-size ::terminated? ::operators]
                        :req-un [::fitness]))

(def descending #(compare %2 %1))
(def ascending compare)

(comment
  (let [one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))
        cfg {::init-pop    #(raw-pop->pop (gen-pop 30 256 one-or-zero))
             ::pop-size 30                                  ;; Check line above
             ::rand-nth rand-nth
             ::elitism-rate 0.1
             ::terminated? (fn [c] (= 256 (apply + c)))
             :fitness      (fn [c] (apply + c))
             ::comparator  descending
             ::reporter    simple-print
             ::selector    #:chickn.selectors{:type        :chickn.selectors/roulette
                                              :random-func rand}
             ::operators   [#:chickn.operators{:type         :chickn.operators/cut-crossover
                                               :rate         0.3
                                               :pointcuts    1
                                               :random-point rand-nth
                                               :random-func  rand}
                            #:chickn.operators{:type          :chickn.operators/rand-mutation
                                               :rate          0.01
                                               :random-func   rand
                                               :mutation-func one-or-zero}]}]
    (:solved? (evolve* cfg 3000))))

(comment
  (let [one-or-zero (fn [& _] (if (> (rand) 0.5) 1 0))
        cfg {::init-pop    #(raw-pop->pop (gen-pop 30 256 one-or-zero))
             ::pop-size 30                                  ;; Check line above
             ::rand-nth rand-nth
             ::elitism-rate 0.1
             ::terminated? (fn [c] (= 0 (apply + c)))
             :fitness      (fn [c] (apply + c))
             ::comparator  ascending
             ::reporter    simple-print
             ::selector    #:chickn.selectors{:type        :chickn.selectors/roulette
                                              :random-func rand}
             ::operators   [#:chickn.operators{:type         :chickn.operators/cut-crossover
                                               :rate         0.3
                                               :pointcuts    1
                                               :random-point rand-nth
                                               :random-func  rand}
                            #:chickn.operators{:type          :chickn.operators/rand-mutation
                                               :rate          0.01
                                               :random-func   rand
                                               :mutation-func one-or-zero}]}]
    (:solved? (evolve* cfg 3000))))

(comment
  (s/explain ::cfg (merge default-cfg {:crossover-rate 2.})))

(comment
  (all-ones-sample))

(comment
  (crossover rand-int 1 [0 0 1 2] [0 0 1 2]))

