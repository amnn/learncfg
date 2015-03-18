(ns cfg.prune
  (:require [cfg.coll-util :refer [queue]]
            [cfg.sat :refer [horn-sat]]
            [cfg.cfg :refer [terminal? non-terminal?
                             non-terminal pattern filterr] :as cfg]
            [cfg.scfg :as scfg]))

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
  [g] (->> g cfg/rule-seq horn-sat))

(defn prune-cfg
  "Given a grammar `g`, remove all the rules that do not contribute to the
  language, and all the rules that are not reachable from the start symbol."
  [g]
  (let [cnts         (contributing-nts g)
        contributes? (fn [rule]
                       (and (cnts (non-terminal rule))
                            (every? #(or (terminal? %) (cnts %))
                                    (pattern rule))))
        g*           (filterr contributes? g)
        rnts         (reachable-nts g*)]
    (filterr (comp rnts non-terminal) g*)))

(defn prune-scfg
  "Given an SCFG `sg` and a probability `p` Remove all rules with
  probabilities below `p`, and then remove any non-terminals which become
  unreachable as a result."
  [p sg]
  (->> sg scfg/rule-seq
       (keep (fn [[r q]] (when (>= q p) r)))
       (reduce cfg/add-rule {})
       prune-cfg
       (scfg/slice sg)))
