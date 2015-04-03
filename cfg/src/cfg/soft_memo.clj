(ns cfg.soft-memo
  (:require [bigml.sampling.simple :as simple]))

(defrecord ^:private MemMap [fresh stale safe])
(defn- mk-map [] (->MemMap {} {} {}))

(defn requery
  "Indicate the need for a requery in `mem` (a *reference* to a memoization map)."
  [mem]
  (swap! mem
         (fn [{:keys [fresh stale safe]}]
           (->MemMap {} fresh (merge safe stale)))))

(defn soft-memoize
  "Memoizes `f` with the option of resetting the cache at a later date."
  [f]
  (let [mem (atom (mk-map))]
    (letfn [(find-in [cache args]
              (find (cache @mem) args))

            (move [from to args]
              (when-let [[_ ret :as entry] (find-in from args)]
                (swap! mem #(-> %
                                (update-in [from] dissoc args)
                                (assoc-in  [to args] ret)))
                entry))

            (refresh [args] (move :safe :fresh args))

            (set-cache [args]
              (let [ret (apply f args)]
                (swap! mem assoc-in [:stale args] ret)
                (move :stale :fresh args)))]
      [mem
       (fn [& args]
         (val (or (find-in :fresh args)
                  (refresh args)
                  (set-cache args))))])))
