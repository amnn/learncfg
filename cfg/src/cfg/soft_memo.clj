(ns cfg.soft-memo
  (:require [bigml.sampling.simple :as simple]))

(defrecord ^:private CacheRecord [conf val])
(defn- mk-c-record [val] (->CacheRecord 1.0 val))

(defrecord ^:private MemMap [fresh stale safe])
(defn- mk-map [] (->MemMap {} {} {}))

(defn requery
  "Indicate the need for a requery in `mem` (a *reference* to a memoization map)."
  [mem]
  (swap! mem
         (fn [{:keys [fresh stale safe]}]
           (->MemMap {} fresh (merge safe stale)))))

(defn- choose [& {:as weights}]
  (first (simple/sample (keys weights)
                        :weigh weights)))

(defn- refresh? [c]
  (choose true c false (- 1 c)))

(defn soft-memoize
  "Memoizes `f` with the option of resetting the cache at a later date."
  [dampen f]
  {:pre [(< 0 dampen 1)]}
  (let [mem (atom (mk-map))]
    (letfn [(find-in [cache args]
              (find (cache @mem) args))

            (move [from to cost args]
              (when-let [[_ rec :as entry] (find-in from args)]
                (swap! mem update-in [from] dissoc args)
                (when (refresh? (:conf rec))
                  (swap! mem assoc-in [to args]
                         (update-in rec [:conf] * cost))
                  entry)))

            (refresh [args] (move :safe :fresh dampen args))

            (set-cache [args]
              (let [ret (apply f args)]
                (swap! mem assoc-in [:stale args]
                       (mk-c-record ret))
                (move :stale :fresh 1.0 args)))]
      [mem
       (fn [& args]
         (-> (or (find-in :fresh args)
                 (refresh args)
                 (set-cache args))
             val :val))])))
