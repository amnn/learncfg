(ns cfg.learn.k-bounded
  (:require [cfg.learn.util :refer :all]
            [cfg.prune :refer [prune-cfg]]
            [cfg.lang  :refer [parse-trees]]
            [cfg.cfg   :refer [cfg add-rule remove-rule cnf-leaf?]]))

(defn learn
  "Takes two functions:

   * `counter*` Takes a grammar, and, if it is the correct one, returns `nil`,
     otherwise, returns a vector of tokens that provides a counter-example
     showing a string that either should be in the language and is not, or
     shouldn't be in the language but is.
   * `member*` Takes a non-terminal, and a sequence of tokens and determines
     whether it is possible to yield the tokens from the non-terminal in
     the target grammar.

  As well as a list of non-terminals `nts`, and attempts to learn a grammar with
  non-terminals in `nts`, terminals `ts` and rules governed by the responses
  from queries to `counter*` and `member*`."
  [member* counter* nts ts]
  (let [member* (memoize member*)]
    (loop [g (init-grammar nts ts)]
      (let [pg (prune-cfg g)]
        (if-let [c (counter* pg)]
          (if-let [t (parse-trees g c)]
            (recur (reduce remove-rule g (diagnose member* t)))
            (recur (reduce add-rule    g (candidates nts c))))
          pg)))))

(defn sample-learn
  "A variant of `interactive-learn` in which the user is presented with
  samples, rather than the grammar, when asked to produce counter-examples.
  Additionally, false-negatives are removed using a corpus of positive data."
  [n corpus nts ts]
  (learn interactive-member
         (sample-counter n corpus)
         nts ts))
