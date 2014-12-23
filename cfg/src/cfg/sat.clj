(ns cfg.sat
  (:require [clojure.set :refer [union]]
            [cfg.coll-util :refer [queue]]))

(defn- invert-rule [[s & rs]]
  (let [nts (->> rs (filter keyword?) (into #{}))]
    (if (pos? (count nts))
      (let [rule (atom [(count nts) s rs])]
        (into {} (map #(vector % #{rule}) nts)))
      {nil #{s}})))

(defn invert-graph
  "Takes a sequence of rules, and inverts it to form a map from non-terminals
  to atoms representing the rules they appear in. Each atom holds a 3-tuple
  containing the number of non-terminals in the rule, the LHS of the rule, and
  the RHS of the rule."
  [rs] (apply merge-with union (map invert-rule rs)))

(defn visit-rule
  "Given the atom representing a rule in the inverted graph representation
  of a CFG, mark it as visited."
  [rule] (swap! rule update-in [0] dec))

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
               (keep #(let [[children nt]
                            (visit-rule %)]
                        (when (zero? children) nt)))
               (into (pop q)))
          (conj! nts nt))
        (persistent! nts)))))
