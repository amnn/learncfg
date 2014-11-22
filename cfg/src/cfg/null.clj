(ns cfg.null
  (:require [clojure.core.reducers :as r]
            [cfg.cfg :refer [non-terminal? rule-seq clean-cfg]]
            [cfg.sat :refer [horn-sat]]))

(defn- null-trans-rule?
  "Predicate indicating whether a rule is null transitive: This means that if
  all the non-terminals in the rule were nullable, then the non-terminal on the
  LHS of the rule is also nullable."
  [[s & rs]] (every? (every-pred non-terminal? #(not= s %)) rs))

(defn- null-trans-graph
  "Returns a representation of the null transitive graph as a sequence of
  rules through which an empty string can potentially propagate in the grammar
  `g`."
  [g] (filter null-trans-rule? (rule-seq g)))

(defn nullable
  "Returns a set of non-terminals in `g` that can be 'erased' i.e. the
  non-terminals from which the empty string can be derived."
  [g] (->> g null-trans-graph horn-sat))

(defn null-free
  "Return a copy of the grammar without rules that introduce epsilons"
  [g] (clean-cfg (r/map #(update-in % [1] disj []) g)))
