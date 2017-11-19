(ns chickn.math)

(defn rnd-index [coll]
  (int (* (rand) (count coll))))                            ;; FIXME make rand configurable

(defn rand-between [min max]
  (+ (* (rand) (- max min)) min))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn std-dev [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (- total 1))
        (Math/sqrt))))

