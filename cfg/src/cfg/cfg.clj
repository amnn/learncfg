(ns cfg.cfg
  (:require [clojure.set :refer [union]]
            [clojure.string :refer [join]]
            [cfg.coll-util :refer :all]
            [cfg.sat :refer [horn-sat]]
            [clojure.core.reducers :as r]))

;;;;;;;;;; Predicates ;;;;;;;;;;

(defn word?
  "Predicate to say whether a derivation is a word."
  [s] (every? symbol? s))

(defn not-word?
  "Complement of `word?`"
  [s] (not-every? symbol? s))

(def terminal? symbol?)
(def non-terminal? keyword?)

(defn cnf-leaf*?
  "Does the given rule qualify as the leaf rule of a CNF grammar."
  [rs] (and (= 1 (count rs))
            (terminal? (first rs))))

(defn cnf-branch*?
  "Does the given rule qualify as the branch rule of a CNF grammar."
  [rs] (and (= 2 (count rs))
            (every? non-terminal? rs)))

(defn cnf-leaf?
  "Does the given rule (in the format returned by rule-seq) qualify as the
  leaf rule of a CNF grammar."
  [[_ & rs]] (cnf-leaf*? rs))

(defn cnf-branch?
  "Does the given rule (in the format returned by rule-seq) qualify as the
  branch rule of a CNF grammar."
  [[_ & rs]] (cnf-branch*? rs))

;;;;;;;;;; Accessors ;;;;;;;;;;

(def ^{:doc "returns the LHS of a rule"} non-terminal first)
(def ^{:doc "returns the RHS of a rule"} pattern      rest)

;;;;;;;;;; Grammar Creation ;;;;;;;;;;

(defn mk-rule
  "Make a rule with LHS `nt` and RHS `rs`."
  [nt rs] (vec (list* nt rs)))

(defn- non-term-rules [[s rss]]
  (map (partial mk-rule s) rss))

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

(defn clean-cfg
  "Ensure there are no empty non-terminals."
  [g] (into {} (r/remove (comp empty? #(nth % 1)) g)))

(defn add-rule
  "Add a singleton rule `s => rs` to the grammar represented by `g`."
  [g [s & rs]]
  (update-in g [s] union #{(vec rs)}))

(defn remove-rule
  "Removes rule `s => rs` from `g` if it exists."
  [g [s & rs]]
  (clean-cfg (update-in g [s] disj (vec rs))))

(defn remove-nt
  "Remove a non-terminal `nt` from grammar `g`"
  [g nt] (dissoc g nt))

;;;;;;;;;; Rule Traversal ;;;;;;;;;;

(defn rule-seq
  "Produces a lazy sequences of rules in `g`, each of the form `[~s, ~@rs] for
  every rule `s => rs` in `g`. Or, if a non-terminal `nt` is also provided,
  creates a lazy sequence of the rules in `g` with `nt` as their LHS."
  ([g] (mapcat non-term-rules g))
  ([g nt] (non-term-rules [nt (g nt)])))

(defn mapr
  "Applies a function to every rule in the grammar, and returns the resulting
  grammar"
  [f g] (reduce add-rule {} (map f (rule-seq g))))

(defn mapr*
  "Applies a function to every rule in the grammar, preserving the
  non-terminal."
  [f g] (mapr (fn [[s & rs]] (mk-rule s (f rs))) g))

(defn filterr
  "Filters the rules in a context free grammar according to a predicate `pred`
  to return a new grammar."
  [pred g] (reduce add-rule {} (filter pred (rule-seq g))))

;;;;;;;;;; CFG Graph Properties ;;;;;;;;;;

(defn reachable-nts
  "Returns the non-terminals we can reach from a given symbol `nt`. If `nt` is
  not provided, it is assumed to be `:S`."
  ([g] (reachable-nts g :S))

  ([g nt]
   (loop [q   (queue nt)
          nts (transient #{})]
     (if (seq q)
       (let [current-nt (peek q)]
         (if-not (nts current-nt)
           (recur (into (pop q)
                        (->> (get g current-nt)
                             (mapcat #(filter non-terminal? %))))
                  (conj! nts current-nt))
           (recur (pop q) nts)))
       (persistent! nts)))))

(defn contributing-nts
  "Returns the non-terminals that can yield strings."
  [g] (->> g rule-seq horn-sat))

(defn prune
  "Given a grammar `g`, remove all the rules that do not contribute to the
  language, and all the rules that are not reachable from the start symbol."
  [g]
  (let [cnts         (contributing-nts g)
        contributes? (partial every? #(or (terminal? %) (cnts %)))
        g*           (filterr contributes? g)
        rnts         (reachable-nts g*)]
    (filterr (comp rnts non-terminal) g*)))

;;;;;;;;;; I/O Helpers ;;;;;;;;;;

(defn show-cfg
  "Provides a textual representation of the grammar `g`."
  [g]
  (letfn [(spaces   [n]    (repeat n \space))
          (flat-str [& ss] (apply str (flatten ss)))

          (print-rule [pad nt rs]
            (let [fmt (str "\n  (%-" pad "s => %s)")
                  sep (flat-str \newline (spaces (+ pad 4)) "|  ")]
              (format fmt nt
                      (->> rs
                           (map #(join \space %))
                           (join sep)))))]
    (let [key-strs (->> g keys (map str))
          pad      (->> key-strs (map count) (reduce max 0))]
      (flat-str "(cfg"
                (map (fn [nt [_ rs]]
                       (print-rule pad nt rs))
                     key-strs g)
                ")"))))

(defn pg
  "CFG Pretty-printer."
  [g] (println (show-cfg g)))
