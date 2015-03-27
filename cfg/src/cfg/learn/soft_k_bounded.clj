(ns cfg.learn.soft-k-bounded
  (:require [cfg.cfg :refer [cfg add-rule remove-rule]]
            [cfg.learn.util :refer :all]
            [cfg.soft-memo :refer [soft-memoize dampen]]
            [cfg.lang :refer [parse-trees]]
            [cfg.prune :refer [prune-cfg]]))

(defn learn
  "Same as `k-bounded/learn` but with extra parameters, `dampen` and `boost` to
  control the soft memoization of `member*`."
  [member* counter* damp-factor boost nts ts]
  (let [[memo member] (soft-memoize boost member*)]
    (loop         [g  (init-grammar nts ts)]
      (let        [pg (prune-cfg g)]
        (if-let   [c  (counter* pg)]
          (if-let [t  (parse-trees g c)]
            (recur (reduce remove-rule g (diagnose member t)))
            (do (dampen damp-factor memo)
                (recur (reduce add-rule g (candidates nts c)))))
          pg)))))

(defn soft-sample-learn
  "Like `sample-learn` but using the soft variant of the learning algorithm."
  [n corpus damp-factor boost nts ts]
  (learn interactive-member
         (sample-counter n corpus)
         damp-factor boost nts ts))
