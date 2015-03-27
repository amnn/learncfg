(ns cfg.learn.klr-k-bounded
  (:require [cfg.cfg :refer [cnf-leaf*?]]
            [cfg.scfg :refer [cfg->scfg normalize
                              make-strongly-consistent
                              make-mutable! freeze!
                              sample] :as scfg]
            [cfg.lang :refer [ml-tree ml-p]]
            [cfg.prune :refer [prune-scfg]]
            [cfg.logistic-regression :refer [mk-classifier learn sigmoid logit]]
            [cfg.learn.util :refer [interactive-member present-samples]]
            [clojure.set :refer [intersection]]
            [clojure.pprint]))

(def ^{:private true :dynamic true} *debug* false)

(defn- init-weights
  "Create an initial classifier for a grammar with non-terminals `nts` and
  terminals `ts`."
  [nts ts]
  (into {}
        (concat
         (for [a nts, b nts, c nts]
           [[a b c] (atom 0.0)])
         (for [a nts, t ts]
           [[a t]   (atom 0.0)]))))

(defn- init-likelihoods
  "Create an initial mutable SCFG for non-terminals `nts`, and terminals `ts`.
  Initially, all with likelihood 0.5."
  [nts ts]
  (let [rules (concat (map list ts)
                      (for [b nts, c nts]
                        (list b c)))]
    (->> (mapcat (fn [nt] (map #(cons nt %) rules)) nts)
         (reduce (fn [sg r] (scfg/add-rule sg r 0.5)) {})
         make-mutable!)))

(defn- classifier->likelihood
  "Link the classifier `c` to the mutable SCFG `sg` such that when the
  coefficients of the classifier are altered."
  [sg {:keys [K ws]}]
  (doseq [[r* atm-a] ws]
    (let [likelihoods
          (for [[r lh] (scfg/rule-seq sg)
                :let [k (K r r*)]
                :when (not (zero? k))]
            [lh k])]
      (add-watch atm-a :likelihood
                 (fn [_ _ old-a new-a]
                   (doseq [[lh* k] likelihoods]
                     (swap! lh* (fn [lh]
                                  (->> (logit lh)
                                       (+ (* (- new-a old-a) k))
                                       sigmoid)))))))))

(defn- snapshot!
  "Convert the given mutable SCFG `sg`, into an immutable SCFG where rules with
  likelihoods less than `p` are removed, and the remaining rules are normalized
  conditional on their non-terminal."
  [p sg] (->> sg freeze! (prune-scfg p) normalize))

(defn- disorder!
  "Scale the likelihoods of every sample in `classifier` by `p`."
  [p classifier]
  (doseq [[_ lh] (:ws classifier)]
    (swap! lh * p)))

(defn- diagnose
  "Given a non-terminal membership predicate `member*` and the most likely
  tree `t` from the parse of an SCFG, return a bad rule used in the tree."
  [member* t]
  (letfn [(children [t]
            (when (and (:lt t) (:rt t))
              ((juxt :lt :rt) t)))]
    (loop [{:keys [rule] :as t} t]
      (if-let [c (some
                  (fn [{:keys [nt yield] :as c}]
                    (when-not (member* nt yield) c))
                  (children t))]
        (recur c)
        rule))))

(defn klr-learn
  "A softened K-bounded learning algorithm that utilises kernel logistic
  regression to deal with errors.
   * `K` The kernel to apply to examples.
   * `member*`
     A function that when given a non-terminal `nt` and a `yield`, returns true
     iff the target grammar can derive `yield` from `nt`.
   * `counter*`
     Given an SCFG `sg`, return a counter-example: `[type toks]`
      - `type` is `:+` if it is a false-positive, and `:-` otherwise.
      - `toks` is the counter-example itself.
     The function can return `nil` if no such counter-example exists.
   * `entropy`
     The factor by which the classifier is disordered if we reach a
     false negative error.
   * `lr-rate`
     The rate of gradient descent of the logistic regression. If this value is
     too high, we will overshoot.
   * `prune-p`
     If a rule exhibits a likelihood less than or equal to this value, we will
     consider that as tantamount to it not being in the SCFG.
   * `nts`
     The list of non-terminals. Used to generate the initial grammar/classifier.
   * `ts`
     The list of terminals. Used to generate the initial grammar/classifier."
  [K member* counter* nts ts & {:keys [entropy lr-rate prune-p]}]
  {:pre [(< 0 entropy 1) (< 0 lr-rate) (< 0 prune-p 1/2)]}

  (let [classifier  (mk-classifier K (init-weights nts ts))
        likelihoods (init-likelihoods nts ts)]
    (classifier->likelihood likelihoods classifier)
    (loop []
      (let [sg (snapshot! prune-p likelihoods)]
        (when *debug*
          (println "\n\n*** LOOP ***\n")
          (println "*** SNAPSHOT")
          (clojure.pprint/pprint sg)

          (println "*** LIKELIHOOD")
          (clojure.pprint/pprint likelihoods)

          (println "*** CLASSIFIER")
          (clojure.pprint/pprint classifier))

        (if-let [[type toks] (counter* sg)]
          (do (when *debug* (println [type toks]))
              (case type
                :+ (learn lr-rate classifier
                          (diagnose member* (ml-tree sg toks)) 0.0)
                :- (disorder! entropy classifier))
              (recur))
          sg)))))

(defn cnf-rk
  "A kernel to compare rules of a context-free grammar in CNF.

   * Rules are considered orthogonal if their LHS is different.
   * Leaf rules are orthogonal to all rules apart from those that are identical
     to them.
   * Non-terminal rules are compared for similarity by counting the number of
     substrings and reverse substrings in common and then renormalizing."
  [[nt1 & rs1] [nt2 & rs2]]
  (letfn [(ss [xs]
            (conj (set xs)
                  (reverse xs)
                  xs))]
    (cond
      (not=  nt1 nt2)  0.0
      (=     rs1 rs2)  1.0
      (some cnf-leaf*?
            [rs1 rs2]) 0.0

      :else
      (-> (intersection
           (ss rs1) (ss rs2))
          count (/ 4.0)))))

(defn id-k
  "A kernel that returns 1.0 if the two given rules are exactly identical, and
  0.0 otherwise. (The Hilbert Space has a dimension for each rule)."
  [r1 r2] (if (= r1 r2) 1.0 0.0))

(defn- scfg-sample
  [sg n] (vec (repeatedly n #(sample sg))))

(defn sample-counter
  "A version of the `counter*` predicate that presents the user with `n`
  samples. Similar to `sample-counter` for the regular k-bounded algorithm, but
  also labels the returned value with whether it is a false-positive or a
  false negative."
  ([sc-rate n corpus] (sample-counter sc-rate n corpus present-samples))

  ([sc-rate n corpus sample-tester]
   (fn [sg]
     (if-let [false-neg
              (some #(when-not (pos? (ml-p sg %)) %)
                    corpus)]
       [:- false-neg]
       (let [sg* (make-strongly-consistent sc-rate sg)]
         (when-let [false-pos (sample-tester (scfg-sample sg* n))]
           [:+ false-pos]))))))

(defn sample-klr-learn
  "Interactively learn a grammar given a `corpus`, set of terminals `ts` and a
  set of non-terminals `nts`, using the `klr-learn` algorithm. Various
  probabilistic parameters are also left modifiable as parameters.

  The parameter `n` is the number of samples that sample based counter-example
  routine should display to the user."
  [nts ts corpus & {:keys [entropy lr-rate prune-p
                           sc-rate samples]}]
  {:pre [(< 1 sc-rate)]}
  (klr-learn
   cnf-rk
   interactive-member
   (sample-counter
    sc-rate samples corpus)
   nts ts
   :entropy entropy
   :lr-rate lr-rate
   :prune-p prune-p))

(comment
  (sample-klr-learn
   [:S :L :R] '[L R]
   '[[L R] [L R L R] [L L R R]
     [L L L R R R] [L R L L R R]
     [L L R R L R] [L L R L R R]]
   :entropy 0.8 :lr-rate 0.1 :prune-p 0.4
   :sc-rate 2   :samples 20))
