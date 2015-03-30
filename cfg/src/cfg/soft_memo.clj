(ns cfg.soft-memo
  (:require [bigml.sampling.simple :as simple]))

(defrecord ^:private CacheRecord [gen last freqs])
(defn- mk-c-record [] (->CacheRecord 0 nil {}))

(defrecord ^:private MemMap [gen cache])
(defn- mk-map [] (->MemMap 1 {}))

(defn requery
  "Indicate the need for a requery in `mem` (a *reference* to a memoization map)."
  [mem] (swap! mem update-in [:gen] inc))

(defn- pick
  "Sample a key from `weights` distributed according to its values."
  [weights] (first (simple/sample (keys weights) :weigh weights)))

(defn soft-memoize
  "Given a function `f`, memoizes it, returning a pair `[mem f*]` of a reference
  to the memoization map `mem` and the memoized function `f*`.

  In `mem` each cached return value keeps track of the last value returned by
  `f` for each list of parameters, as well as the frequencies of all previously
  returned values.

  When `f*` is called with arguments `args`, we return the last value cached
  with probability `(/ n N)` where `n` is the frequency at which the last value
  has been seen, and `N` is the total calls of `(apply f args)`, otherwise we
  call `f` directly again."
  [f]
  (let [mem (atom (mk-map))]
    [mem
     (fn [& args]
       (let [mem-gen (:gen @mem)

             {:keys [gen last freqs] :as rec}
             (get-in @mem [:cache args]
                     (mk-c-record))

             ret (cond->> freqs
                   (< gen mem-gen) (merge {::requery 1})
                   :always         pick)]
         (if (= last ret)
           last
           (let [v (apply f args)]
             (swap! mem assoc-in
                    [:cache args]
                    (-> rec
                        (assoc :gen mem-gen)
                        (assoc :last v)
                        (update-in [:freqs v]
                                   #(inc (or % 0)))))
             v))))]))
