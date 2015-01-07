(ns cfg.sat
  (:require [clojure.set :refer [union]]
            [cfg.coll-util :refer [queue]]
            [cfg.invert :refer [invert-graph visit-rule]]))

(defn horn-sat
  "Take a sequence of rules `rs` and returns the set of satisfying non-terminals
  for the underlying HORN-SAT formula."
  [rs]
  (let [sat-graph (invert-graph rs)]
    (loop [q   (apply queue (get sat-graph nil))
           nts (transient #{})]
      (if-let [nt (peek q)]
        (recur
          (->> (sat-graph nt)
               (keep #(let [{:keys [not-visited nt]}
                            (visit-rule %)]
                        (when (zero? not-visited) nt)))
               (into (pop q)))
          (conj! nts nt))
        (persistent! nts)))))
