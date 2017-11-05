(ns chickn.util)

(defn now-millis []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))
