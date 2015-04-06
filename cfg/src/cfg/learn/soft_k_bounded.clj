(ns cfg.learn.soft-k-bounded
  (:require [cfg.cfg :refer [cfg add-rule remove-rule]]
            [cfg.learn.util :refer :all]
            [cfg.soft-memo :refer [soft-memoize requery]]
            [cfg.lang :refer [parse-trees]]
            [cfg.prune :refer [prune-cfg]]))

(defn learn
  "Same as `k-bounded/learn` but with extra parameters, `dampen` and `boost` to
  control the soft memoization of `member*`."
  [dampen member* counter* nts ts]
  (let [[memo member] (soft-memoize dampen member*)]
    (loop         [g  (init-grammar nts ts)]
      (let        [pg (prune-cfg g)]
        (if-let   [c  (counter* pg)]
          (if-let [t  (parse-trees g c)]
            (recur (reduce remove-rule g (diagnose member t)))
            (do (requery memo)
                (recur (reduce add-rule g (candidates nts c)))))
          pg)))))

(defn soft-sample-learn
  "Like `sample-learn` but using the soft variant of the learning algorithm."
  [dampen n corpus nts ts]
  (learn dampen interactive-member
         (sample-counter n corpus)
         nts ts))
