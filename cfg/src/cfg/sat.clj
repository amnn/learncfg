(ns cfg.sat
  (:require [clojure.set :refer [union]]
            [clojure.core.reducers :as r]
            [cfg.list-util :refer [queue]]))

(defn- invert-rule [[s & rs]]
  (let [nts (->> rs (filter keyword?) (into #{}))]
    (if (pos? (count nts))
      (let [rule (atom [(count nts) s])]
        (into {} (map #(vector % #{rule}) nts)))
      {nil #{s}})))

(defn- invert-graph
  "Takes a sequence of rules, and inverts it to form a map from non-terminals
  to atoms representing the rules they appear in. Each atom holds a pair
  containing the number of non-terminals in the rule, and the LHS of the rule."
  [rs] (apply merge-with union (map invert-rule rs)))

(defn horn-sat
  "Take a sequence of rules `rs` and returns the set of satisfying non-terminals
  for the underlying HORN-SAT formula."
  [rs]
  (let [sat-graph (invert-graph rs)
        visit     #(swap! % update-in [0] dec)]
    (loop [q   (apply queue (get sat-graph nil))
           nts (transient #{})]
      (if (seq q)
        (let [nt (peek q)]
          (recur
            (->> (sat-graph nt)
                 (r/map visit)
                 (r/filter (comp zero? first))
                 (r/map second)
                 (into (pop q)))
            (conj! nts nt)))
        (persistent! nts)))))
