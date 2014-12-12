(ns cfg.scfg
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :as m]
            [cfg.list-util :refer [map-v]]
            [cfg.cfg :refer [cnf-leaf*?]]))

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
  (let [order (vec (keys sg))
        sparse-m
        (map-v (fn [rs]
                 (reduce (fn [m [r p]]
                           (if (cnf-leaf*? r)
                             (update-in m [:v] + p)
                             (reduce #(let [path [:M %2]
                                            p*   (get-in %1 path 0)]
                                        (assoc-in %1 path (+ p* p)))
                                     m r)))
                         {:M {} :v 0}
                         rs))
               sg)]
    {:order order
     :M (array :vectorz
               (for [i order]
                 (for [j order]
                   (get-in sparse-m [i :M j] 0))))

     :v (array :vectorz
               (for [i order]
                 (get-in sparse-m [i :v])))}))

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
