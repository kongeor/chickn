(ns chickn.core)

(defn rank [{f :fitness} pop]
  (map f pop))

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
