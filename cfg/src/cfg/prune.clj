(ns cfg.prune
  (:require [cfg.coll-util :refer [queue]]
            [cfg.sat :refer [horn-sat]]
            [cfg.cfg :refer [terminal? non-terminal?
                             non-terminal rule-seq filterr]]))

(defn reachable-nts
  "Returns the non-terminals we can reach from a given symbol `nt`. If `nt` is
  not provided, it is assumed to be `:S`."
  ([g] (reachable-nts g :S))

  ([g nt]
   (loop [q   (queue nt)
          nts (transient #{})]
     (if (seq q)
       (let [current-nt (peek q)]
         (if-not (nts current-nt)
           (recur (into (pop q)
                        (->> (get g current-nt)
                             (mapcat #(filter non-terminal? %))))
                  (conj! nts current-nt))
           (recur (pop q) nts)))
       (persistent! nts)))))

(defn contributing-nts
  "Returns the non-terminals that can yield strings."
  [g] (->> g rule-seq horn-sat))

(defn prune
  "Given a grammar `g`, remove all the rules that do not contribute to the
  language, and all the rules that are not reachable from the start symbol."
  [g]
  (let [cnts         (contributing-nts g)
        contributes? (partial every? #(or (terminal? %) (cnts %)))
        g*           (filterr contributes? g)
        rnts         (reachable-nts g*)]
    (filterr (comp rnts non-terminal) g*)))
