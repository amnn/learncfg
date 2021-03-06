(ns cfg.hop
  (:require [clojure.data.priority-map :refer [priority-map]]
            [cfg.coll-util :refer [map-kv*]]
            [cfg.cfg :refer [rule-seq non-terminal?]]
            [cfg.invert :refer [invert-graph visit-rule]]))

(defn- remove-self-loops
  "Given a CFG `g`, return a new CFG where all self loops had been removed."
  [g]
  (map-kv* (fn [nt rs]
             (->> rs
                  (remove #(some #{nt} %))
                  (into (empty rs))))
           g))

(defn hop-counts
  "Given a grammar `g`, return a map from non-terminals in `g` to the minimum
  number of derivation steps required to get them to a terminal string."
  [g]
  (let [hop-graph (->> g remove-self-loops
                       rule-seq invert-graph)
        no-hops   #(vector % 0)]
    (loop [counts (transient {})
           q (->> (hop-graph nil)
                  (map no-hops)
                  (into (priority-map)))]
      (if-let [[nt hops] (peek q)]
        (if (get counts nt)
          (recur counts (pop q))
          (let [new-counts (assoc! counts nt hops)]
            (recur
             new-counts
             (into (pop q)
                   (for [r (hop-graph nt)
                         :let  [old-hops (get q (:nt @r))]
                         :when (or (not old-hops) (> old-hops hops))
                         :let  [{:keys [not-visited nt rule]}
                                (visit-rule r)]
                         :when (zero? not-visited)]
                     [nt (->> (filter non-terminal? rule)
                              (map new-counts)
                              (reduce +) inc)])))))
        (persistent! counts)))))

(defn best-rules
  "Given a grammar `g`, return a subgrammar containing the rules for each
  non-terminal with the lowest hop count."
  [g]
  (let [counts (hop-counts g)
        rule-hop
        (fn [rule]
          (if-let [nts (seq (filter non-terminal? rule))]
            (->> nts (map counts)
                 (reduce +) inc)
            0))]
    (map-kv*
      (fn [nt rs]
        (let [hops (get counts nt)]
          (->> rs
               (filter #(= hops (rule-hop %)))
               (into #{}))))
      g)))
