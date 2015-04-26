(ns cfg.soft-memo
  (:require [bigml.sampling.simple :as simple]))

(defrecord ^:private CacheRecord [gen mode modal-freq freqs])
(defn- mk-c-record [gen val] (->CacheRecord gen val 1 {val 1}))

(defrecord ^:private MemMap [gen cache])
(defn- mk-map [] (->MemMap 0 {}))

(defn requery
  "Indicate the need for a requery in `mem` (a *reference* to a memoization map)."
  [mem] (swap! mem update-in [:gen] inc))

(defn soft-memoize
  "Memoizes `f` with the option of resetting the cache at a later date."
  [f]
  (let [mem (atom (mk-map))]
    (letfn [(set-mode [rec ret freq]
              (-> rec
                  (assoc :mode ret)
                  (assoc :modal-freq freq)))

            (update-mode [rec ret]
              (let [freq (get-in rec [:freqs ret] 0)]
                (cond-> rec
                  (>= freq (:modal-freq rec))
                  (set-mode ret freq))))

            (update-cache [rec ret]
              (update-in rec [:freqs ret]
                         #(inc (or % 0))))]
      [mem
       (fn [& args]
         (let [gen (:gen @mem)]
           (swap! mem update-in [:cache args]
                  (fn [rec]
                    (let [ret (delay (apply f args))]
                      (if-not rec
                        (mk-c-record gen @ret)
                        (if (< (:gen rec) gen)
                          (-> rec
                              (update-cache @ret)
                              (update-mode  @ret)
                              (assoc :gen gen))
                          rec)))))
           (get-in @mem [:cache args :mode])))])))
