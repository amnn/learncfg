(ns cfg.cfg
  (refer-clojure :exclude [interleave])
  (require [clojure.set :refer [union]]
           [cfg.util :refer :all]))

(defn- arrow? [x] (= '=> x))

(defn- last-i [coll]
  (dec (count coll)))

(defn- split-rules [bodies]
  (letfn [(add-tok [rules tok]
            (if (= '| tok)
              (conj rules [])
              (update-in
                rules [(last-i rules)]
                conj tok)))]
    (reduce add-tok [[]] bodies)))

(defn- rule->map
  "Converts individual rules as described in the docstring of `cfg` into a
  singleton map for consumption by the `cfg` macro."
  [[s => & bodies]]
  {:pre [(arrow? =>)
         (keyword? s)]}
  {s (into #{} (split-rules bodies))})

(defmacro cfg
  "Combines the rules given as parameters together to form a context free
  grammar. Rules are lists with the following format:

    (:S => A B :C D | E F :G H)

  This defines two rules, for the non-terminal `:S`. The first being
  `:S => A B :C D` and the second being `:S => E F :G H`. Rules may be
  empty, i.e. `(:S => )`. The `:S` non-terminal is implicitly the starting
  non-terminal."
  [& rules]
  `'~(->> rules
          (map rule->map)
          (apply merge-with union)))

(defmacro rule
  "Creates a single `s => rs` mapping to be used in a CFG."
  [s => & rs]
  {:pre [(arrow? =>)
         (keyword? s)
         (not-any? #{'|} rs)]}
  `'[~s ~@rs])

(defn add-rule
  "Add a singleton rule `s => rs` to the grammar represented by `g`."
  [g [s & rs]]
  (update-in g [s] union #{(vec rs)}))

(defn- clean-cfg [g]
  (into {} (remove (comp empty? val) g)))

(defn remove-rule
  "Removes rule `s => rs` from `g` if it exists."
  [g [s & rs]]
  (clean-cfg (update-in g [s] disj (vec rs))))

(defn- non-term-rules [[s rss]]
  (map #(->> % (list* s) vec) rss))

(defn rule-seq
  "Produces a lazy sequences of rules in `g`, each of the form `[~s, ~@rs] for
  every rule `s => rs` in `g`."
  [g]
  (mapcat non-term-rules g))

(defn non-terms [r]
  "Returns the list of non-terminals along with the indices they occur at."
  (keep-indexed #(when (keyword? %2) [% %2]) r))

(declare derivation-seq)

(defn expand-rule [g r]
  "Fills the non-terminals in the rule `r` with the derivation-seqs from the
  grammar `g` and the appropriate non-terminals."
  (let [nts (non-terms r)]
    (lazy-seq
      (if-let [is (seq (map first nts))]
        (let [bs (->> nts
                      (map #(derivation-seq g (second %)))
                      combine)]
          (cons r (map #(apply assoc r
                               (interleave is %))
                       bs)))
        (list r)))))

(defn derivation-seq
  "Returns a list of derivations from non-terminal `s` in grammar `g`."
  [g s]
  (->> (seq (g s))
       (map #(expand-rule g %))
       interleave*))

(defn lang-seq
  "Returns a sequence of strings in the language.
  **NOTE** If the language is ambiguous, this sequence will contain
  duplicates."
  [g]
  (->> (derivation-seq g :S)
       (map flatten)
       (filter #(every? symbol? %))))
