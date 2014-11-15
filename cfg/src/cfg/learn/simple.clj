(ns cfg.learn.simple
  (:require [clojure.set :refer [union]]
            [clojure.core.reducers :as r]
            [clojure.data.priority-map :refer [priority-map]]
            [cfg.list-util :refer [replace-coll queue]]
            [cfg.lang :refer [deriv-len]]
            [cfg.cfg :refer [mapr mk-rule add-rule rule-seq
                             remove-nt non-terminal pattern]]))

(defn- complexity
  "Given a grammar and a sample, score it in terms of its complexity.
  Complexity is a score given by the number of non-terminals and rules
  in a given grammar, as well as the size of the derivations of each sample in
  the given grammar."
  [sample g]
  (let [n    (count g)
        r    (count (rule-seq g))
        lens (map (deriv-len g) sample)]
    (+ r n (reduce + lens))))

(defn- priority-record [sample]
  (juxt identity (partial complexity sample)))

(defn- prioritise
  "Convert a sequence of grammars into a priority map, according to their
  complexity"
  [sample gs]
  (->> gs
       (map (priority-record sample))
       (into (priority-map))))

(defn merge-nts
  "Combine the rules for `nt1` and `nt2` under one terminal (`nt1`), replacing
  all instances of `nt2` in the rules with `nt1`."
  [g nt1 nt2]
  {:pre [(contains? g nt1)
         (contains? g nt2)]}
  (let [r1 (g nt1), r2 (g nt2)
        nt-map {nt2 nt1}
        new-nts #(get nt-map % %)]
    (mapr (partial map new-nts)
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

(defn- sym-pair-freqs
  "Return the frequencies of pairs of adjacent symbols in the rule."
  [rs] (frequencies (map vector rs (rest rs))))

(defn pair-freqs
  "Returns the most frequently occurring pair of symbols in rules in `g`."
  [g]
  (->> (rule-seq g)
       (map (comp sym-pair-freqs pattern))
       (apply merge-with +)))

(defn- fresh-nt [] (-> "R" gensym keyword))

(defn- nt-pairs
  "Given a grammar g, return all pairs of non-terminals in g."
  [g]
  (let [nts (vec (keys g))
        num-nts (count nts)]
    (for [j (range num-nts)
          i (range (inc j) num-nts)
          :let [m (get nts i)
                n (get nts j)]]
      (if (= :S n) [n m] [m n]))))

(defn possible-nt-merges
  "Returns a sequence of grammars that result from merging pairs of
  non-terminals."
  [g]
  (->> (nt-pairs g)
       (map (partial apply merge-nts g))))

(defn possible-pair-extractions
  "Returns a sequence of grammars that result from extracting pairs from
  rules."
  [g]
  (->> (pair-freqs g)
       (keep #(when (<= 2 (val %))
                (extract-rule
                  g (mk-rule (fresh-nt)
                             (key %)))))))

(defn- token-set
  "Returns the set of tokens in use in the grammar"
  [sample]
  (->> sample
       (map (partial into #{}))
       (reduce union)))

(defn- sample-rule
  "Convert a sample string into a rule on the starting state. Assumes there are
  nominal rules for each token."
  [sample] (mk-rule :S (map keyword sample)))

(defn- nominal-rules
  "Converts a collection of terminals into a sequence of trivial rules.
  (Lifting the terminals to non-terminals)"
  [ts] (map #(mk-rule (keyword %) (vector %)) ts))

(defn- init-grammar
  "Initial grammar when learning a particular sample."
  [sample]
  (->> sample
       (map sample-rule)
       (concat (nominal-rules (token-set sample)))
       (reduce add-rule {})))

(def ^:dynamic *beam-width* 3)

(defn- expand-frontier
  "Take a function `expand-fn` from grammars to sequences of grammars, and a
  `sample` of test cases, and creates a function that when given the frontier,
  expands it using the given function, prioritises it."
  [sample expand-fn]
  (fn [frontier]
    (->> frontier
         (mapcat (comp expand-fn first))
         (prioritise sample))))

(defn learn
  "Given a sample, produce a context free grammar that recognises it as a
  subset of its language set."
  [sample]
  (let [init        (init-grammar sample)
        frontier-fn {:merge   (expand-frontier
                                sample possible-nt-merges)
                     :extract (expand-frontier
                                sample possible-pair-extractions)}]
    (loop [frontier [[init (complexity sample init)]]
           mode     (cycle [:merge :extract])]
      (let [[g* c*]      (apply min-key second frontier)
            expand       (frontier-fn (first mode))
            new-frontier (->> frontier expand
                              (filter (fn [[_ c]] (> c* c)))
                              (take *beam-width*))]
        (if (seq new-frontier)
          (recur new-frontier
                 (rest mode))
          g*)))))
