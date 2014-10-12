(ns cfg.null
  (:require [clojure.set :refer [union]]
            [clojure.core.reducers :as r]
            [cfg.cfg :refer [rule-seq clean-cfg]]
            [cfg.list-util :refer [queue]]))

(defn- null-trans-rule?
  "Predicate indicating whether a rule is null transitive: This means that if
  all the non-terminals in the rule were nullable, then the non-terminal on the
  LHS of the rule is also nullable."
  [[s & rs]] (every? (every-pred keyword? #(not= s %)) rs))

(defn- null-trans-graph
  "Returns a representation of the null transitive graph as a sequence of
  rules through which an empty string can potentially propagate in the grammar
  `g`."
  [g] (filter null-trans-rule? (rule-seq g)))

(defn- invert-rule [[s & rs]]
  (if (pos? (count rs))
    (let [nts (into #{} rs)
          rule (atom [(count nts) s])]
      (into {} (map #(vector % #{rule}) nts)))
    {nil #{s}}))

(defn- invert-graph
  "Takes a sequence of rules, and inverts it to form a map from non-terminals
  to atoms representing the rules they appear in. Each atom holds a pair
  containing the number of non-terminals in the rule, and the LHS of the rule."
  [rs] (apply merge-with union (map invert-rule rs)))

(defn nullable
  "Returns a set of non-terminals in `g` that can be 'erased' i.e. the
  non-terminals from which the empty string can be derived."
  [g]
  (let [ntg (->> g null-trans-graph invert-graph)
        visit #(swap! % update-in [0] dec)]
    (loop [q (apply queue (get ntg nil))
           nulls (transient #{})]
      (if (seq q)
        (let [nt (peek q)]
          (recur
            (->> (ntg nt)
                 (r/map visit)
                 (r/filter (comp zero? first))
                 (r/map second)
                 (into (pop q)))
            (conj! nulls nt)))
        (persistent! nulls)))))

(defn null-free
  "Return a copy of the grammar without rules that introduce epsilons"
  [g] (clean-cfg (r/map #(update-in % [1] disj []) g)))
