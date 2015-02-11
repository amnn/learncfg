(ns cfg.learn.test-rig
  (:require [cfg.learn.k-bounded :as kb]
            [cfg.learn.klr-k-bounded :refer [klr-learn cnf-rk id-k] :as klr-kb]
            [cfg.lang :refer [parse-trees]]))

(defn- inject-counter
  [ctr f]
  (fn [& args]
    (swap! ctr inc)
    (apply f args)))

(defn- inject-printer
  [prt-fn f]
  (fn [& args]
    (let [y (apply f args)]
      (prt-fn args y)
      y)))

(defn- member-print
  [[nt yield] ans]
  (println
   (str nt " =>* " yield "? "
        (if ans \y \n))))

(defn- counter-print
  [[g] ans]
  (println "counter*")
  (clojure.pprint/pprint g)
  (println
   (if ans
     (str "\t=> " ans)
     "DONE!")))

(defn sample-test-rig
  "Runs a learning algorithm automatically, returning the resultant
  grammar, and the number of calls made to the various querying routines.

  If the `verbose?` flag is set to `true` (it defaults to `false`), also
  prints the query questions and responses as they are made.

  Takes the `learn`-ing algorithm, the membership predicate `member*`,
  the counter-example predicate `counter*`, a `corpus` of positive
  examples, and a count of samples `n`."

  ([learn member* counter* n corpus]
   (sample-test-rig learn member* counter* n corpus false))

  ([learn member* counter* n corpus verbose?]
   (let [counter-calls (atom 0)
         member-calls  (atom 0)

         result
         (learn
          (cond->> member*
            :always  (inject-counter member-calls)
            verbose? (inject-printer member-print))

          (cond->> (counter* n corpus
                             (fn [samples]
                               (some #(when-not (member* :S %) %)
                                     (sort-by count samples))))
            :always  (inject-counter counter-calls)
            verbose? (inject-printer counter-print)))]
     {:grammar       result
      :member-calls  @member-calls
      :counter-calls @counter-calls})))

(defn k-bounded-rig
  [g corpus & {:keys [verbose? samples]}]
  (let [member*
        (fn [nt yield]
          (boolean
           (parse-trees g nt yield)))

        nts (keys g)]
    (sample-test-rig
     #(kb/learn %1 %2 nts)
     member* kb/sample-counter
     samples corpus verbose?)))

(defn klr-k-bounded-rig
  [g ts corpus
   & {:keys [entropy prune-p
             lr-rate sc-rate
             verbose? samples
             kernel]
      :or {kernel cnf-rk}}]
  (let [member*
        (fn [nt yield]
          (boolean
           (parse-trees g nt yield)))

        nts (keys g)]
    (sample-test-rig
     #(klr-learn kernel
                 %1 %2 nts ts
                 :entropy entropy
                 :prune-p prune-p
                 :lr-rate lr-rate)
     member*
     (partial klr-kb/sample-counter
              sc-rate)
     samples corpus verbose?)))

(comment
  ;; Balanced Parens
  (k-bounded-rig
   (cfg
    (:S => :L :R | :S :S)
    (:L => L | :L :S | :S :L)
    (:R => R | :R :S | :S :R))

   '[[L R] [L R L R] [L L R R]
     [L L L R R R] [L R L L R R]
     [L L R R L R] [L L R L R R]]
   :verbose? true
   :samples 30)

  (klr-k-bounded-rig
   (cfg
    (:S => :L :R | :S :S)
    (:L => L | :L :S | :S :L)
    (:R => R | :R :S | :S :R))
   '[L R]
   '[[L R] [L R L R] [L L R R]
     [L L L R R R] [L R L L R R]
     [L L R R L R] [L L R L R R]]
   :verbose? true
   :kernel   id-k
   :entropy  0.5
   :lr-rate  0.5
   :prune-p  0.4
   :sc-rate  2
   :samples  30)

  ;; (AB)+
  (k-bounded-rig
   (cfg
    (:S  => :S :S | :A :B)
    (:A  => A)
    (:B  => B))
   '[[A B] [A B A B]
     [A B A B A B]
     [A B A B A B A B]]
   :verbose? true
   :samples  30)

  (klr-k-bounded-rig
   (cfg
    (:S  => :S :S | :A :B)
    (:A  => A)
    (:B  => B))
   '[A B]
   '[[A B] [A B A B]
     [A B A B A B]
     [A B A B A B A B]]
   :verbose? true
   :kernel   id-k
   :entropy  0.5
   :lr-rate  0.5
   :prune-p  0.4
   :sc-rate  2
   :samples  30)

  ;; A^nB^n
  (k-bounded-rig
   (cfg
    (:S  => :A :S*)
    (:S* => B | :S :B)
    (:A => A) (:B => B))
   '[[A B] [A A B B]
     [A A A B B B]
     [A A A A B B B B]]
   :verbose? true
   :samples 30)

  (klr-k-bounded-rig
   (cfg
    (:S  => :A :S*)
    (:S* => B | :S :B)
    (:A => A) (:B => B))
   '[A B]
   '[[A B] [A A B B]
     [A A A B B B]
     [A A A A B B B B]]
   :verbose? true
   :kernel   id-k
   :entropy  0.5
   :lr-rate  0.5
   :prune-p  0.4
   :sc-rate  2
   :samples  30)

  ;; A^nB^mC^(n+m)
  (k-bounded-rig
   (cfg
    (:S  => :A :S+ | :B :S+)
    (:S+ => :S :C | C)
    (:A  => A) (:B => B) (:C => C))
   '[[A C] [B C]
     [A A C C] [A B C C] [B B C C]
     [A A A C C C] [A A B C C C]
     [A B B C C C] [B B B C C C]]
   :verbose? true
   :samples  30)

  (klr-k-bounded-rig
   (cfg
    (:S  => :A :S+ | :B :S+)
    (:S+ => :S :C | C)
    (:A  => A) (:B => B) (:C => C))
   '[A B C]
   '[[A C] [B C]
     [A A C C] [A B C C] [B B C C]
     [A A A C C C] [A A B C C C]
     [A B B C C C] [B B B C C C]]
   :verbose? true
   :kernel   id-k
   :entropy  0.5
   :lr-rate  0.5
   :prune-p  0.4
   :sc-rate  2
   :samples  30))
