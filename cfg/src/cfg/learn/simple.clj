(ns cfg.learn.simple
  (:require [clojure.set :refer [union]]))

(defn merge-nts
  "Combine the rules for `nt1` and `nt2` under one terminal (`nt1`), replacing
  all instances of `nt2` in the rules with `nt1`."
  [g nt1 nt2]
  {:pre [(contains? g nt1)
         (contains? g nt2)]}
  (let [r1 (g nt1), r2 (g nt2)
        nt-map {nt2 nt1}
        new-nts #(get nt-map % %)]
    (into
      {} (map (fn [kvp]
                (update-in kvp [1]
                  #(into #{}
                         (map (partial mapv new-nts) %))))
              (-> g
                  (assoc nt1 (union r1 r2))
                  (dissoc nt2))))))
