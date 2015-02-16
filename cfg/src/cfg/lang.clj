(ns cfg.lang
  (:require [cfg.null :refer [nullable null-free]]
            [clojure.set :refer [union]]
            [clojure.core.reducers :as r]
            [cfg.cfg :refer [add-rule terminal? non-terminal?
                             branch?] :as cfg]
            [cfg.scfg :as scfg]
            [cfg.coll-util :refer [queue]]))

(defrecord ^:private Item
  [rule start offset deriv-len toks])

(defn- new-item
  "Constructs a fresh Earley Item for a rule `r` starting at `start`"
  [r start] (->Item r start 0 1 []))

(defn- init-items
  "Create a sequence of new Earley Items for a non-terminal `nt` and grammar
  `g` starting at the index `i`."
  [g nt i] (map #(new-item % i) (cfg/rule-seq g nt)))

(defn- classify
  "Determines what should be done to the given Earley Item."
  [{[_ & rs] :rule offset :offset}]
  {:pre  [(not (neg? offset))]
   :post [(#{::shift ::reduce ::predict} %)] }
  (if-let [el (get (vec rs) offset)]
    (condp apply [el]
      terminal?     ::shift
      non-terminal? ::predict)
    ::reduce))

(defn- non-terminal
  "Get the non-terminal on the LHS of the rule in the item."
  [i] (get-in i [:rule 0]))

(defn- reduxn-key
  "Gives the start position and the non-terminal symbol of the item.
  This information can be used to find the items that are completed by this
  one."
  [i] ((juxt :start non-terminal) i))

(defn- processed-key
  "If the rule, starting position, offset and tokens consumed are the same
  between items being processed in a single iteration, then they are the same
  item, and one can be pruned."
  [i] ((juxt :rule :start :offset :toks) i))

(defn- next-sym
  "Get the next symbol in the item."
  [{:keys [rule offset]}]
  (get rule (inc offset)))

(defn- shift
  "Move the item's cursor forward over a given token or sequence of tokens."
  [item toks]
  (-> item
      (update-in [:offset] inc)
      (update-in [:toks]
                 #(if (coll? %2)
                    (reduce conj %1 %2)
                    (conj %1 %2))
                 toks)))

(defn- inc-deriv-len
  "Add the given value to the derivation length of an Earley Item."
  [item len] (update-in item [:deriv-len] + len))

(defrecord ^:private EarleyState
  [reduxns items complete])

(def ^:private item-seed
  (new-item '[DONE :S] 0))

(def ^:private success-key
  (reduxn-key item-seed))

(defn- initial-state [has-empty?]
  (->EarleyState
    {} (queue item-seed)
    (if has-empty?
      {success-key #{[]}}
      {})))

(defn- perform-shift
  "Shifts an item over tokens and adds it to the end of a queue"
  [q i toks] (conj q (shift i toks)))

(defn- enqueue-shift
  "Takes an `item`, shifts it over some tokens, `toks`, and adds it to the end
  of the item queue in the `state`."
  [state item toks]
  (update-in state [:items]
             perform-shift item toks))

(defn- perform-reduxns
  "Creates a sequence of all the waiting reductions after they have been
  reduced. by the given finished `item`."
  [state item]
  (let [path [:reduxns (reduxn-key item)]
        {:keys [toks deriv-len]} item]
    (map #(-> % (shift toks)
              (inc-deriv-len deriv-len))
         (get-in state path))))

(defn- associate-reduxn
  "Add an `item` as waiting for a reduction (keyed by `r-key`) in the parser
  state `state."
  [state r-key item]
  (let [path [:reduxns r-key]]
    (if (get-in state path)
      (update-in state path conj item)
      (assoc-in  state path #{item}))))

(defn- complete-item
  "Mark an item as completed in the given `state`."
  [state item]
  (let [path [:complete (reduxn-key item)]
        {:keys [toks deriv-len]} item]
    (if (get-in state path)
      (update-in state path conj [toks deriv-len])
      (assoc-in  state path {toks deriv-len}))))

(defn- reset-state
  "Reset the completions and item queue of the state, ready to consume the
  next token."
  [state]
  (-> state
      (assoc :items (queue)
             :complete {})))

(defn- token-consumer
  "Takes:
    * g: the CFG, containing no rules containing epsilons.
    * nullable?: a predicate that determines whether the non-terminals in `g`
      can be erased (by deriving the empty string from them)
    * shift?: a predicate that given an index and a symbol, determines whether
      the recogniser can shift at the given index if that index contains the
      given symbol.
  Returns a function that updates an EarleyState by consuming an extra index
  position in the input."
  [g nullable? shift?]
  (fn [{:keys [items] :as state} index]
    (loop [processed? #{}, items items
           state (reset-state state)]
      (if (seq items)
        (let [item (peek items)
              p-key (processed-key item)]
          (if (processed? p-key)
            (recur processed? (pop items) state)
            (case (classify item)

              ::shift
              (recur (conj processed? p-key)
                     (pop items)
                     (let [tok (next-sym item)]
                       (if (shift? index tok)
                         (enqueue-shift state item tok)
                         state)))

              ::reduce
              (recur (conj processed? p-key)
                     (into (pop items)
                           (perform-reduxns state item))
                     (complete-item state item))

              ::predict
              (let [nt         (next-sym item), r-key [index nt]
                    predicted? (contains? (:reduxns state) r-key)
                    items      (pop (if (nullable? nt)
                                      (let [item* (inc-deriv-len item 1)]
                                        (perform-shift items item* []))
                                      items))]
                (recur
                  (conj processed? p-key)
                  (if predicted?
                    items
                    (into items (init-items g nt index)))
                  (associate-reduxn state r-key item))))))
        state))))

(defn- toks->shift?
  "Given a list of tokens, produces the appropriate shift-predicate"
  [toks]
  (let [toks-v (vec toks)]
    (fn [i sym]
      (when-let [t (get toks-v i)]
        (= t sym)))))

(defn deriv-len
  "Returns a function that, given a sequence of tokens, returns the length of
  the derivation of the grammar `g` that produces that sequence, if such a
  sequence exists. Otherwise, returns nil."
  [g]
  (let [nullable?     (nullable g)
        g             (null-free g)
        consume-token (partial token-consumer g nullable?)
        init-state    (initial-state (nullable? :S))]
    (fn [toks]
      ;; Incrementing (count toks) because completion steps for the kth token
      ;; occur whilst processing the (k+1)th.
      (let [index-range    (-> toks count inc range)
            token-consumer (consume-token (toks->shift? toks))
            final-state    (reduce token-consumer init-state index-range)]
        (when-let [len (get-in final-state [:complete success-key toks])]
          ;; Decrementing the length so that we account for the nominal
          ;; DONE -> S rule we add to the grammar to simplify dealing with
          ;; S -> ε rules.
          (dec len))))))

(defn in-lang
  "Returns the recogniser function for the grammar `g`. A grammar recognises
  a sequence of tokens if there is a non-nil derivation length for that
  sequence."
  [g] (comp some? (deriv-len g)))

(defn lang-seq
  "Returns an infinite sequence of strings in the language defined by the
  grammar `g`."
  [g]
  (let [nullable?     (nullable g)
        g             (null-free g)
        consume-token (token-consumer g nullable? (constantly true))
        init-state    (initial-state (nullable? :S))

        [active-states idle-states]
        (->> (reductions consume-token init-state (range))
             rest (split-with (comp not empty? :items)))]
    (->> (concat active-states (take 1 idle-states))
         (mapcat #(get-in % [:complete success-key]))
         (map first))))

(defrecord ^:private Terminal [t])

(defn- parse-tree*
  "Generalisation of the CYK algorithm, for calculating the parse trees of
  CNF grammars (see `(doc parse-tree)` for details), wherein, the following
  must be supplied by the caller:
    * `->branch`/`->leaf`
      Data constructors for the tree nodes.
      Usage: `(->branch nt rule yield lt rt)`
             `(->leaf   nt rule yield)`
    * `branches`/`leaves`
      Rules of the grammar, split according to whether they are branching or
      not.
    * `merge-fn`
      When two different sub-trees rooted at the same non-terminal produce the
      same yield, they are given to this function to merge/choose the best
      sub-tree.
    * `rule`
      A function that extracts the rule from the aforementioned `branches` and
      `leaves`.
    * `ts`
      The sequence of tokens to parse.
    * `root`
      The root non-terminal."
  [& {:keys [->branch ->leaf merge-fn rule
             branches leaves root ts]}]
  (let [toks (vec ts)
        n    (count toks)

        t-map (->> leaves
                   (map (fn [l]
                          (let [[nt t] (rule l)]
                            {t {nt l}})))
                   (apply merge-with merge))

        subtok (fn [start len]
                 (subvec toks start
                         (+ start len)))

        partials
        {1 (into {} (for [j (range n)
                          :let [[t :as yield] (subtok j 1)]]
                      [j (into {} (for [[nt l] (get t-map t)]
                                    [nt (->leaf nt l yield)]))]))}
        child
        (fn [p len start sym]
          (if (terminal? sym)
            (when (and (= len 1)
                       (= (get toks start) sym))
              (->Terminal sym))
            (get-in p [len start sym])))

        build-partial
        (fn [p [i j k branch]]
          (let        [[a b c]  (rule branch)]
            (if-let   [bt       (child p k j b)]
              (if-let [ct       (child p (- i k) (+ j k) c)]
                (let  [new-node (->branch a branch (subtok j i) bt ct)]
                  (if-let [node (get-in p [i j a])]
                    (update-in p [i j a] merge-fn new-node)
                    (assoc-in  p [i j a] new-node)))
                p)
              p)))]

    (-> (reduce build-partial partials
                (for [i (range 2 (inc n))  ;; Subsequence length
                      j (range (- n i -1)) ;; Start position
                      k (range 1 i)        ;; Split point
                      b branches]          ;; Rule
                  [i j k b]))
        (get-in [n 0 root]))))

(defrecord ^:private MultiBranch [nt yield children])
(defrecord ^:private MultiLeaf   [nt yield children])

(defn parse-trees
  "Given a grammar `g` in Chomsky Normal Form(1), and a sequence of tokens,
  `ts`, returns all derivation trees for, `g` recognising `ts` if any exist
  or `nil` otherwise. Optionally, a `root` non-terminal may be provided, which
  will be the non-terminal whose rules form the root node of the trees. If
  one is not provided, `:S` is picked by default.

  (1) All rules in a CFG in CNF are of the form:

  * A => BC
  * A => a

  For non-terminals `A`, `B`, `C` and terminals `a`."

  ([g ts] (parse-trees g :S ts))

  ([g root ts]
   (let [{branches true leaves false}
         (group-by branch?
                   (cfg/rule-seq g))]
     (parse-tree*
      :branches branches, :leaves leaves
      :root root, :ts ts

      :->branch
      (fn [nt rule yield lt rt]
        (->MultiBranch nt yield #{[rule lt rt]}))

      :->leaf
      (fn [nt rule yield]
        (->MultiLeaf nt yield #{[rule]}))

      :merge-fn
      (fn [b1 b2]
        (update-in b1 [:children] union
                   (:children b2)))

      :rule identity))))

(defrecord ^:private PBranch [nt rule p yield lt rt])
(defrecord ^:private PLeaf   [nt rule p yield])

(defn ml-tree
  "Given an SCFG `sg` in Chomsky Normal Form, and a sequence of tokens, `ts`,
  returns the derivation tree of maximum likelihood for `g`, recognising `ts`
  if any exists, or `nil` otherwise. Optionally, a `root` non-terminal may be
  provided, which will be the non-terminal whose rule forms the root of the
  tree. If one is not provided, `:S` is picked as the default."

  ([sg ts] (ml-tree sg :S ts))

  ([sg root ts]
   (let [{branches true leaves false}
         (group-by (comp branch? first)
                   (scfg/rule-seq sg))]
     (parse-tree*
      :branches branches, :leaves leaves
      :root root, :ts ts

      :->branch
      (fn [nt [rule p] yield lt rt]
        (->PBranch nt rule (* p (or (:p lt) 1.0) (or (:p rt) 1.0))
                   yield lt rt))

      :->leaf
      (fn [nt [rule p] yield]
        (->PLeaf nt rule p yield))

      :merge-fn
      (fn [b1 b2]
        (max-key :p b1 b2))

      :rule first))))

(defn ml-p
  "Returning the probability of generating `ts` with SCFG `sg`."
  [sg ts] (or (:p (ml-tree sg ts)) 0.0))

(defrecord ^:private MultiPBranch [nt yield p children])
(defrecord ^:private MultiPLeaf   [nt yield p children])

(defn inside-p
  "Given an SCFG `sg` in Chomsky Normal Form, and a sequence of tokens `ts`,
  returns all the derivation trees, for `sg` recognising `ts`, annotated with
  the inside probability of generating `ts` with `sg`."

  ([sg ts] (inside-p sg :S ts))

  ([sg root ts]
   (let [{branches true leaves false}
         (group-by (comp branch? first)
                   (scfg/rule-seq sg))]
     (parse-tree*
      :branches branches, :leaves leaves
      :root root, :ts ts

      :->branch
      (fn [nt [rule p] yield lt rt]
        (->MultiPBranch nt yield (* p (:p lt) (:p rt))
                        #{[rule lt rt]}))

      :->leaf
      (fn [nt [rule p] yield]
        (->MultiPLeaf nt yield p #{[rule]}))

      :merge-fn
      (fn [b1 {cs2 :children p2 :p}]
        (-> b1
            (update-in [:p] + p2)
            (update-in [:children] union cs2)))

      :rule first))))
