(ns cfg.invert
  (:require [clojure.set :refer [union]]
            [cfg.cfg :refer [non-terminal?]]))

(defrecord ^:private Rule
  [not-visited nt rule])

(defn- invert-rule [[s & rs]]
  (let [nts (->> rs (filter non-terminal?) (into #{}))]
    (if (pos? (count nts))
      (let [rule (atom (->Rule (count nts) s rs))]
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
  [rule] (swap! rule update-in [:not-visited] dec))
