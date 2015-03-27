(ns cfg.soft-memo
  (:require [bigml.sampling.simple :as simple]))

(defrecord ^:private MemMap [uncertainty vals])
(defn- mk-map [] (->MemMap 1.0 {}))

(defn dampen
  "Scales the overall confidence of all cached return values in `mem` (a
  *reference* to a memoization map) by a factor of `dampen`."
  [factor mem]
  {:pre [(> 1 factor)]}
  (swap! mem update-in
         [:uncertainty]
         * factor))

(defn- pick
  "Sample a key from `weights` distributed according to its values."
  [& {:as weights}] (first (simple/sample (keys weights) :weigh weights)))

(defn- choose
  "Decide whether to fetch from or refresh the cache, according to confidence
  `c`."
  [c] (pick ::fetch c, ::refresh (- 1 c)))

(defn- mix
  "Linearly interpolate between `x` and `y`, with ratio `r`."
  [x r y] (+ x (* r (- y x))))

(defn soft-memoize
  "Given a function `f`, memoizes it, returning a pair `[mem f*]` of a reference
  to the memoization map `mem` and the memoized function `f*`.

  In `mem` each cached return value has an associated confidence value,
  0 <= c <= 1, initially c = 1.

  When `f*` is called, if there is a cached value `z` with confidence `c`, it
  is returned with probability `c`, and the original function is called again
  with probability `1-c`. If the original function agrees with the cached value,
  then the cached value's confidence is increased by a factor of `boost`."
  [boost f]
  {:pre [(< 0 boost) (<= boost 1)]}
  (let [mem (atom (mk-map))]
    (letfn [(set-cache [args c val]
              (swap! mem assoc-in
                     [:vals args]
                     [c val])
              val)

            (compare-and-set-cache [u args new-val]
              (swap! mem update-in
                     [:vals args]
                     (fn [[old-c old-val]]
                       (if (= old-val new-val)
                         [(mix old-c boost (/ 1 u)) old-val]
                         [(mix 0 boost old-c)       new-val])))
              (get-in @mem [:vals args 1]))]
      [mem
       (fn [& args]
         (let [u (:uncertainty @mem)]
           (if-let [[c val] (get-in @mem [:vals args])]
             (let [confidence (* c u)]
               (case (choose confidence)
                 ::fetch val
                 ::refresh
                 (compare-and-set-cache
                  u args (apply f args))))
             (set-cache args u (apply f args)))))])))
