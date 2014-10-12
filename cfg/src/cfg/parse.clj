(ns cfg.parse
  (:require [clojure.set :refer [union]]
            [clojure.core.reducers :as r]
            [cfg.cfg :refer [rule-seq null-free terminal? non-terminal?]]
            [cfg.list-util :refer [queue]]))

(defn- null-trans-rule?
  "Predicate indicating whether a rule is null transitive: This means that if
  all the non-terminals in the rule were nullable, then the non-terminal on the
  LHS of the rule is also nullable."
  [[s & rs]] (every? (every-pred keyword? #(not= s %)) rs))

(defn- null-trans-graph
  "Returns a representation of the null transitive graph as a sequence of
  rules through which an empty string can potentially propagate in the grammar
  `g`."
  [g] (filter null-trans-rule? (rule-seq g)))

(defn- invert-rule [[s & rs]]
  (if (pos? (count rs))
    (let [nts (into #{} rs)
          rule (atom [(count nts) s])]
      (into {} (map #(vector % #{rule}) nts)))
    {nil #{s}}))

(defn- invert-graph
  "Takes a sequence of rules, and inverts it to form a map from non-terminals
  to atoms representing the rules they appear in. Each atom holds a pair
  containing the number of non-terminals in the rule, and the LHS of the rule."
  [rs] (apply merge-with union (map invert-rule rs)))

(defn nullable
  "Returns a set of non-terminals in `g` that can be 'erased' i.e. the
  non-terminals from which the empty string can be derived."
  [g]
  (let [ntg (->> g null-trans-graph invert-graph)
        visit #(swap! % update-in [0] dec)]
    (loop [q (apply queue (get ntg nil))
           nulls (transient #{})]
      (if (seq q)
        (let [nt (peek q)]
          (recur
            (->> (ntg nt)
                 (r/map visit)
                 (r/filter (comp zero? first))
                 (r/map second)
                 (into (pop q)))
            (conj! nulls nt)))
        (persistent! nulls)))))

(defrecord ^:private Item
  [rule start offset])

(defn- init-items
  "Create a sequence of new Earley Items for a non-terminal `nt` and grammar
  `g` starting at the index `i`."
  [g nt i] (map #(->Item % i 0) (rule-seq g nt)))

(defn- classify
  "Determines what should be done to the given Earley Item."
  [{[_ & rs] :rule offset :offset}]
  {:pre  [(not (neg? offset))]
   :post [(#{:shift :reduce :predict} %)] }
  (if-let [el (get (vec rs) offset)]
    (condp apply [el]
      terminal?     :shift
      non-terminal? :predict)
    :reduce))

(defn- non-terminal
  "Get the non-terminal on the LHS of the rule in the item."
  [i] (get-in i [:rule 0]))

(defn- reduxn-key
  "Gives the start position and the non-terminal symbol of the item.
  This information can be used to find the items that are completed by this
  one."
  [i] ((juxt :start non-terminal) i))

(defn- next-sym
  "Get the next symbol in the item."
  [{:keys [rule offset]}]
  (get rule (inc offset)))

(defn- shift
  "Move the item's cursor forward."
  [i] (update-in i [:offset] inc))

(defrecord ^:private ParserState
  [index reduxns items complete])

(def ^:private item-seed
  (->Item '[PARSE :S] 0 0))

(def ^:private success-key
  (reduxn-key item-seed))

(defn- initial-state [has-empty?]
  (->ParserState
    0 {} (queue item-seed)
    (if has-empty?
      #{success-key}
      #{})))

(defn- perform-shift
  "Shifts an item and adds it to the end of a queue"
  [q i] (conj q (shift i)))

(defn- enqueue-shift
  "Takes an `item`, shifts it, and adds it to the end of the item queue in the
  `state`."
  [state item] (update-in state [:items] perform-shift item))

(defn- perform-reduxns
  "Creates a sequence of all the waiting reductions after they have been
  reduced."
  [state r-key]
  (map shift (get-in state [:reduxns r-key])))

(defn- associate-reduxn
  "Add an `item` as waiting for a reduction (keyed by `r-key`) in the parser
  state `state."
  [state r-key item]
  (let [path [:reduxns r-key]]
    (if (get-in state path)
      (update-in state path conj item)
      (assoc-in  state path #{item}))))

(defn- complete-reduxns
  "Mark a reduction key as completed in the given `state`."
  [state r-key] (update-in state [:complete] conj r-key))

(defn- reset-state
  "Reset the completions and item queue of the state, ready to consume the
  next token."
  [state]
  (-> state
      (assoc :items (queue)
             :complete #{})
      (update-in [:index] inc)))

(defn recogniser
  "Returns the recogniser function for the grammar `g`."
  [g]
  (let [nullable?  (nullable g)
        g          (null-free g)
        init-state (initial-state (nullable? :S))]
    (letfn [(consume-token [{:keys [index items] :as state} tok]
              (loop [processed? #{}
                     items items
                     state (reset-state state)]
                (if (seq items)
                  (let [item (peek items)]
                    (if (processed? item)
                      (recur processed? (pop items) state)
                      (case (classify item)

                        :shift
                        (recur (conj processed? item)
                               (pop items)
                               (if (= tok (next-sym item))
                                 (enqueue-shift state item)
                                 state))

                        :reduce
                        (let [r-key (reduxn-key item)]
                          (recur (conj processed? item)
                                 (into (pop items)
                                       (perform-reduxns state r-key))
                                 (complete-reduxns state r-key)))

                        :predict
                        (let [nt         (next-sym item), r-key [index nt]
                              predicted? (contains? (:reduxns state) r-key)
                              items      (pop (if (nullable? nt)
                                                (perform-shift items item)
                                                items))]
                          (recur
                            (conj processed? item)
                            (if predicted?
                              items
                              (into items (init-items g nt index)))
                            (associate-reduxn state r-key item))))))
                  state)))]
      (fn [toks]
        (-> (->> (conj (vec toks) nil)
                 (reduce consume-token init-state)
                 :complete)
            (contains? success-key))))))
