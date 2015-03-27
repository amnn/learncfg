(ns cfg.learn.k-bounded
  (:require [cfg.learn.util :refer :all]
            [cfg.prune :refer [prune-cfg]]
            [cfg.lang  :refer [parse-trees]]
            [cfg.cfg   :refer [cfg add-rule remove-rule cnf-leaf?]]))

(defn- candidate
  "Given a multimap of non-terminals to terminals they cannot yield (the
  `blacklist`), a sequence of non-terminals, `nts` and a vector of tokens,
  `toks`, give a collection of rules that, when added to the grammar associated
  with `blacklist` and `nts`, will allow it to recognise `toks`."
  [nts blacklist toks]
  (for [t toks, nt nts,
        :let  [leaf [nt t]]
        :when (not (blacklist leaf))]
    leaf))

(defn- init-grammar
  "Given a sequence of non-terminals `nts`, create the CNF grammar containing
  all possible branches of those non-terminals (without any leaf nodes)."
  [nts]
  (reduce add-rule (cfg)
          (for [a nts b nts c nts]
            [a b c])))

(defn learn
  "Takes two functions:

   * `counter*` Takes a grammar, and, if it is the correct one, returns `nil`,
     otherwise, returns a vector of tokens that provides a counter-example
     showing a string that either should be in the language and is not, or
     shouldn't be in the language but is.
   * `member*` Takes a non-terminal, and a sequence of tokens and determines
     whether it is possible to yield the tokens from the non-terminal in
     the target grammar.

  As well as a list of non-terminals `nts`, and attempts to learn a grammar
  with non-terminals in `nts` and rules governed by the responses from queries
  to `counter*` and `member*`."
  [member* counter* nts]
  (let [member* (memoize member*)]
    (loop [g (init-grammar nts), blacklist #{}]
      (let [pg (prune-cfg g)]
        (if-let [c (counter* pg)]
          (if-let [t (parse-trees g c)]
            (let [bad-rules  (diagnose member* t)
                  bad-leaves (filter cnf-leaf? bad-rules)]
              (recur (reduce remove-rule g bad-rules)
                     (into blacklist bad-leaves)))

            (let [new-rules (candidate nts blacklist c)]
              (recur (reduce add-rule g new-rules)
                     blacklist)))
          pg)))))

(defn sample-learn
  "A variant of `interactive-learn` in which the user is presented with
  samples, rather than the grammar, when asked to produce counter-examples.
  Additionally, false-negatives are removed using a corpus of positive data."
  [n corpus nts]
  (learn interactive-member
         (sample-counter n corpus)
         nts))
