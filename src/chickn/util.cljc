(ns chickn.util)

(defn noop [& args])

(defn val-cycle
  "Returns a func that upon each invocation
  returns each passed element.

  Used for dev and testing."
  [& vals]
  (let [c (atom (cycle vals))]
    (fn [& _]
      (let [f (first @c)
            n (next @c)]
        (reset! c n)
        f))))

(defn now-millis []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn fancy-printer []                                      ;; TODO fix
  (let [ind-tokens [\| \\ \- \/]
        sz (dec (count ind-tokens))
        i (atom 0)
        inc-tk #(let [p (if (= @i sz) (reset! i 0) (swap! i inc))]
                  (get ind-tokens p))]
    (fn [pop]
      (print (inc-tk) "Iter: " 0 "Fitness:" (:best-fitness pop) "Best:" (:best-chromo pop)))))

(defn simple-printer [{:keys [iteration best-fitness time best-chromo]}]
  (println "Iter:" iteration "Fitness:" best-fitness (str "Time: " time "ms") "Best:" best-chromo))

