(ns cfg.learn.simple
  (:require [clojure.set :refer [union]]
            [cfg.list-util :refer [replace-coll]]
            [cfg.cfg :refer [mapr mk-rule add-rule
                             remove-nt non-terminal pattern]]))

(defn merge-nts
  "Combine the rules for `nt1` and `nt2` under one terminal (`nt1`), replacing
  all instances of `nt2` in the rules with `nt1`."
  [g nt1 nt2]
  {:pre [(contains? g nt1)
         (contains? g nt2)]}
  (let [r1 (g nt1), r2 (g nt2)
        nt-map {nt2 nt1}
        new-nts #(get nt-map % %)]
    (mapr new-nts
          (-> (remove-nt g nt2)
              (assoc nt1 (union r1 r2))))))

(defn extract-rule
  "Takes all instances of `rs` in rules of `g` and replaces them with a
  non-terminal `s`. Then adds a rule `s => rs` to `g`."
  [g [s & rs :as rule]]
  (-> (mapr (fn [rule*]
              (if (= rule rule*)
                rule*
                (mk-rule (non-terminal rule*)
                         (replace-coll
                           rs s (pattern rule*)))))
            g)
      (add-rule rule)))
