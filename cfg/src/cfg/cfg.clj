(ns cfg.cfg
  (require [clojure.set :refer [union]]))

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
  empty, i.e. `(:S => )`"
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
  (update-in g [s] conj (vec rs)))

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
