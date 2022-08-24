(ns chickn.reinsertion
  (:require [clojure.spec.alpha :as s]))

;; -----
;; Spec

(defmulti reinsertion-type ::type)

(defmethod reinsertion-type ::pure [_]
  (s/keys :req [::type]))

(defmethod reinsertion-type ::elitist [_]
  (s/keys :req [::type]))

(defmethod reinsertion-type ::uniform [_]
  (s/keys :req [::type]))

(s/def ::reinsertion (s/multi-spec reinsertion-type ::type))

;; -------
;; Reinsertion impl

(defn pure [cfg]
  (fn [{:keys [parents offspring leftover]}]
    {:offspring offspring}))

;; TODO this should accept a rate and we should adjust the pop size later
(defn elitist [{:keys [::rate] :as cfg}]
  (fn [{:keys [:chickn.core/comparator :chickn.core/pop-size]}
       {:keys [parents offspring leftover]}]
    (let [old (concat parents leftover)
          n   (int (* (count old) rate))
          survivors (->> old
                         (sort-by :fitness comparator)
                         (take n))]
      ;; survivors need to come first
      ;; for the following adjustment ops
      (concat survivors offspring))))


;; constructor funcs

(defmulti ->reinsertion ::type)

(defmethod ->reinsertion ::pure [cfg])
(defmethod ->reinsertion ::elitist [cfg]
  (elitist cfg))
(defmethod ->reinsertion ::uniform [cfg])
