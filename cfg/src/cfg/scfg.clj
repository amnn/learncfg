(ns cfg.scfg
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :as m]
            [cfg.coll-util :refer [map-v]]
            [cfg.cfg :refer [terminal?]]))

(defn cfg->scfg
  "Given a CFG `g`, produce an SCFG with the same rules as `g`, and uniform
  probabilities. `g` must be well-formed. i.e. All its non-terminals must
  posess atleast one rule."
  [g]
  (map-v (fn [rs]
           (let [p (->> (count rs) (/ 1) double)]
             (->> (map #(vector % p) rs)
                  (into {}))))
         g))

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
  (->> sg
       (map-v (partial
                reduce
                (fn [row [rule p]]
                  (reduce (fn [row sym]
                            (let [col (if (terminal? sym) ::T sym)
                                  p*  (get row col 0)]
                              (assoc row col (+ p p*))))
                          row rule))
                {}))))

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
