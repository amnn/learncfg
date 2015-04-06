(ns cfg.scfg
  (:require [clojure.core.matrix :refer [array inverse mmul identity-matrix]]
            [clojure.core.matrix.operators :as m]
            [bigml.sampling.simple :as simple]
            [cfg.coll-util :refer [map-v map-kv]]
            [cfg.hop :refer [best-rules]]
            [cfg.graph :refer [transpose children scc]]
            [cfg.cfg :refer [mk-rule terminal? non-terminal?] :as cfg]))

(defn cfg->scfg
  "Given a CFG `g`, produce an SCFG with the same rules as `g`, and uniform
  probabilities. `g` must be well-formed. i.e. All its non-terminals must
  possess atleast one rule."
  [g]
  (map-v (fn [rs]
           (let [p (->> (count rs) (/ 1) double)]
             (into {} (map #(vector % p) rs))))
         g))

(defn scfg->cfg
  "Given an SCFG `sg`, produce a CFG with the same rules as `g`."
  [sg] (map-v (comp set keys) sg))

(defn rule-p
  "Given an SCFG `sg` and a rule return its associated probability, or `nil`
  if it doesn't exist."
  [sg [lhs & rhs]] (get-in sg [lhs (vec rhs)] 0.0))

(defn rule-seq
  "Given an `sg`, returns a sequence of [rule probability] pairs in `sg`.
  If an `nt` is also provided, then only rules from that non-terminal will be
  given."
  ([sg]
   (for [[nt rs] sg
         [r p]   rs]
     [(mk-rule nt r) p]))

  ([sg nt]
   (for [rs (get sg nt {})
         [r p] rs]
     [(mk-rule nt r) p])))

(defn add-rule
  "Add rule `nt => r` with probability `p` to SCFG `sg`."
  [sg [nt & r] p] (assoc-in sg [nt (vec r)] p))

(defn slice
  "Given an SCFG `sg` and a CFG `g`, return an SCFG with the rules from `g`
  and the probabilities from `sg`."
  [sg g]
  (->> g cfg/rule-seq
       (map (fn [r] [r (rule-p sg r)]))
       (reduce (partial apply add-rule) {})))

(defn sample
  "Given an SCFG `sg`, returns a string generated according to its
  distribution. Rooted at a non-terminal `nt`, which defaults to `:S`."
  ([sg] (sample sg :S))

  ([sg nt]
   (letfn [(pick-rule [nt]
             (let [rules (get sg nt {})]
               (first (simple/sample (keys rules)
                                     :weigh rules))))

           (recur-left [[nt & rhs]]
             (concat (pick-rule nt) rhs))]
     (loop [lhs [] rhs (list nt)]
       (if (seq rhs)
         (let [[l r] (split-with terminal? (recur-left rhs))]
           (recur (concat lhs l) r))
         (vec lhs))))))

(defn e-graph
  "A sparse adjacency list for the directed graph representation of the
  expectation system (`e-system`, see below) for an SCFG.

  For an SCFG with `n` non-terminals, the graph contains `n+1` nodes: one for
  each non-terminal, and a sink node representing all terminals.

  An edge `A -> B` exists between non-terminals `A` and `B` if there exists a
  rule in `sg` from A to a string containing `B`.

  An edge `A -> B` exists between non-terminal `A` and terminal
  sink node `::T` if there is a rule in `sg` from `A` to a string containing
  a terminal.

  Each edge `A -> B` is labeled with the probability of seeing a B after
  transitioning from A."
  [sg]
  (map-v (partial
          reduce
          (fn [row [rule p]]
            (reduce (fn [row sym]
                      (let [col (if (terminal? sym) ::T sym)
                            p*  (get row col 0)]
                        (assoc row col (+ p p*))))
                    row rule))
          {})
         sg))

(defn e-system
  "Given an SCFG `sg` in CNF return a record containing a matrix `:M`, a
  vector `:v` and an ordering `:order`.

  `:M` and `:v` combined describe the expected length of derivations for a
  given non-terminal.

  Let `l` be the vector of expected lengths s.t. `l_i` is the expected word
  length for derivations rooted by the non-terminal `:order_i`, then we have
  the following:

  l = M.l + v"
  [sg]
  (let [order    (vec (keys sg))
        sparse-m (e-graph sg)]
    {:order order
     :M (array :vectorz
               (for [i order]
                 (for [j order]
                   (get-in sparse-m [i j] 0))))

     :v (array :vectorz
               (for [i order]
                 (get-in sparse-m [i ::T] 0)))}))

(defn strongly-consistent?
  "An SCFG is Strongly Consistent if its expected derivation length is finite.
  This is true if and only if there is a unique solution for l, which is
  positive, for the linear system:

  l = M.l + v

  Where l, M and v are defined as in the doc-string of `e-system`."
  [sg]
  (let [{:keys [M v order]} (e-system sg)
        n (count order)
        I (identity-matrix :vectorz n)]
    (boolean
      (when-let [inv (inverse (m/- I M))]
        (->> (mmul inv v)
             (every? pos?))))))

(defn make-mutable!
  "Wrap probabilities in an SCFG `sg` with Atoms, so that they can be
  modified."
  [sg] (map-v (partial map-v atom) sg))

(defn freeze!
  "Freeze a mutable SCFG `sg` to its current probabilities."
  [sg] (map-v (partial map-v deref) sg))

(defn normalize!
  "Ensure that the probabilities of a mutable SCFG `sg` all sum to one
  (conditional on the non-terminal)."
  [sg]
  (doseq [[_ rules] sg
          :let [sum (->> rules
                         (map (fn [[_ p]] @p))
                         (reduce +))]
          [_ p] rules]
    (swap! p / sum))
  sg)

(defn normalize
  "Given an SCFG `sg` return a new SCFG in which all probabilities conditional
  on a particular non-terminal sum to 1."
  [sg]
  (map-v (fn [rules]
           (let [sum (reduce + (vals rules))]
             (map-v #(/ % sum) rules)))
         sg))

(defn- slice-component
  "Given an SCFG `sg` and a sequence of non-terminals `nts` returns a new
  SCFG containing only the non-terminals `nts`. With any non-terminals
  in rules in `sg` replaced by a special sentinel terminal."
  [sg nts]
  (let [valid-nts (set nts)
        new-term  #(gensym `T)
        dangling? #(and (non-terminal? %)
                        (not (valid-nts %)))]
    (->> nts
         (select-keys sg)
         (map-v
           (partial map-kv
                    (fn [k v]
                      [(mapv #(if (dangling? %)
                                (new-term)
                                %)
                             k)
                       v]))))))

(defn- slice-ps
  "Given an SCFG `sg` and a CFG `g`, return a sequence of probabilities from
  `sg` corresponding to rules in `g`."
  [sg g]
  (map #(rule-p sg %)
       (cfg/rule-seq g)))

(defn- componentize
  "Split a grammar according to the strongly connected components of its
  expectation graph. If rules in one of the components refer to a non-terminal
  in another, then that symbol in the rule is replaced with a special
  terminal symbol."
  [sg]
  (->> sg freeze! e-graph
       (map-v #(dissoc % ::T))
       scc
       (map (partial slice-component sg))))

(defn- make-strongly-consistent*
  "Helper function to make the given mutable SCFG strongly consistent. It
  assumes there is only one strongly connected component in the expectation
  graph of `sg`. Rate of convergence is given by `rate."
  [rate sg]
  (let [best-ps
        (->> sg scfg->cfg
             best-rules
             (slice-ps sg)
             delay)]
    (while (not (strongly-consistent? (freeze! sg)))
      (doseq [p (force best-ps)]
        (swap! p * rate))
      (normalize! sg))))

(defn make-strongly-consistent
  "Make the SCFG `sg` strongly consistent. If `sg` is not strongly consistent,
  converge on a solution with rate `rate` (defaulting to 2)."
  ([sg] (make-strongly-consistent 2 sg))
  ([rate sg]
   (let [sg* (make-mutable! sg)]
     (doseq [sub-g (componentize sg*)]
       (make-strongly-consistent* rate sub-g))
     (freeze! sg*))))
