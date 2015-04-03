(ns cfg.learn.test-rig
  (:require [clojure.pprint :refer [pprint]]
            [bigml.sampling.simple :as simple]
            [cfg.learn.util :refer [sample-counter]]
            [cfg.learn.k-bounded :as kb]
            [cfg.learn.soft-k-bounded :as skb]
            [cfg.lang :refer [yields?]]))

(defn- inject-error
  [err verbose? pred]
  (letfn [(should-err []
            (first
             (simple/sample
              [true false]
              :weigh {true err,
                      false (- 1 err)})))]
    (fn [& args]
      (let [b (apply pred args)]
        (if (should-err)
          (do
            (when verbose? (println "*** ERROR ***"))
            (not b))
          b)))))

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
  (pprint g)
  (println
   (if ans
     (str "\t=> " ans)
     "DONE!")))

(defn sample-test-rig
  "Runs a learning algorithm automatically, returning the resultant
  grammar, and the number of calls made to the various querying routines.

  If the `verbose?` flag is set to `true` (it defaults to `false`), also
  prints the query questions and responses as they are made.

  The `error` parameter controls how often the test rig makes a mistake in
  answering a query, by default it has value `0.0` i.e. It makes no errors.

  Takes the `learn`-ing algorithm, the membership predicate `member*`,
  the counter-example predicate `counter*`, a `corpus` of positive
  examples, and a count of samples `n`."

  ([learn member* counter* n corpus
    & {:keys [verbose? error]
       :or   {verbose? false
              error 0.0}}]

   (let [counter-calls (atom 0)
         member-calls  (atom 0)

         result
         (learn
          (cond->> member*
            :always  (inject-error error verbose?)
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

(defn grammar-rig
  [learn g ts corpus {:keys [verbose? error samples]
                      :or   {verbose? false
                             error    0.0}}]
  (let [member*
        (fn [nt yield]
          (boolean
           (yields? g nt yield)))

        nts (keys g)]
    (sample-test-rig
     #(learn %1 %2 nts ts)
     member* sample-counter
     samples corpus
     :verbose? verbose?
     :error    error)))

(defn k-bounded-rig
  [g ts corpus & {:as params}]
  (grammar-rig kb/learn g ts corpus params))

(defn soft-k-bounded-rig
  [g ts corpus & {:as params}]
  (grammar-rig skb/learn g ts corpus params))

(comment
  (require '[cfg.cfg :refer :all]
           '[cfg.learn.test-rig :refer :all]
           :reload-all)
  ;; Balanced Parens 1
  (k-bounded-rig
   (cfg
    (:S => :L :R)
    (:S => :L :R | :S :S)
    (:L => < | :L :S | :S :L)
    (:R => > | :R :S | :S :R))

   '[< >]

   '[[< >] [< > < >] [< < > >]
     [< < < > > >] [< > < < > >]
     [< < > > < >] [< < > < > >]]
   :verbose? true
   :error    0.2
   :samples  30)

  (soft-k-bounded-rig
   (cfg
    (:S => :L :R | :S :S)
    (:L => < | :L :S | :S :L)
    (:R => > | :R :S | :S :R))

   '[< >]

   '[[< >] [< > < >] [< < > >]
     [< < < > > >] [< > < < > >]
     [< < > > < >] [< < > < > >]]
   :verbose?    true
   :error       0.2
   :samples     30)

  ;; Balanced Parens loose
  (k-bounded-rig
   (cfg
    (:S  => < :S1 | :S :S)
    (:S1 => :S > | >))

   '[< >]

   '[[< >] [< > < >] [< < > >]
     [< < < > > >] [< > < < > >]
     [< < > > < >] [< < > < > >]]
   :verbose? true
   :error   0.2
   :samples 30)

  (soft-k-bounded-rig
   (cfg
    (:S  => < :S1 | :S :S)
    (:S1 => :S > | >))

   '[< >]

   '[[< >] [< > < >] [< < > >]
     [< < < > > >] [< > < < > >]
     [< < > > < >] [< < > < > >]]
   :verbose?    true
   :error       0.2
   :samples     30)

  ;; (AB)+
  (k-bounded-rig
   (cfg (:S  => :AB :S)
        (:AB => :A :B)
        (:A  => A)
        (:B  => B))
   '[A B]
   '[[A B] [A B A B]
     [A B A B A B]
     [A B A B A B A B]]
   :verbose? true
   :samples  30)

  (soft-k-bounded-rig
   (cfg (:S  => :AB :S)
        (:AB => :A :B)
        (:A  => A)
        (:B  => B))

   '[A B]

   '[[A B] [A B A B]
     [A B A B A B]
     [A B A B A B A B]]
   :verbose?    true
   :error       0.05
   :samples     30)

  ;; (AB)+ loose
  (k-bounded-rig
   (cfg (:S  => :S :S | A B))
   '[A B]
   '[[A B] [A B A B]
     [A B A B A B]
     [A B A B A B A B]]
   :verbose? true
   :samples  30)

  (soft-k-bounded-rig
   (cfg (:S => :S :S | A B))

   '[A B]

   '[[A B] [A B A B]
     [A B A B A B]
     [A B A B A B A B]]
   :verbose?    true
   :error       0.05
   :samples     30)


  ;; A^nB^n
  (k-bounded-rig
   (cfg
    (:S  => :A :S*)
    (:S* => :B | :S :B)
    (:A  => A)
    (:B  => B))

   '[A B]

   '[[A B] [A A B B]
     [A A A B B B]
     [A A A A B B B B]]
   :verbose? true
   :samples 30)

  (k-bounded-rig
   (cfg
    (:S  => :A :S*)
    (:S* => :B | :S :B)
    (:A  => A)
    (:B  => B))

   '[A B]

   '[[A B] [A A B B]
     [A A A B B B]
     [A A A A B B B B]]
   :verbose?    true
   :error       0.05
   :samples     30)

  ;; A^nB^n loose
  (k-bounded-rig
   (cfg
    (:S  => A :S*)
    (:S* => B | :S B))

   '[A B]

   '[[A B] [A A B B]
     [A A A B B B]
     [A A A A B B B B]]
   :verbose? true
   :samples 30)

  (k-bounded-rig
   (cfg
    (:S  => A :S*)
    (:S* => B | :S B))

   '[A B]

   '[[A B] [A A B B]
     [A A A B B B]
     [A A A A B B B B]]
   :verbose?    true
   :error       0.05
   :samples     30)

  ;; A^nB^mC^(n+m)
  (k-bounded-rig
   (cfg
    (:S  => :A :S+ | :B :S+)
    (:S+ => :S :C | :C)
    (:A  => A)
    (:B  => B)
    (:C  => C))

   '[A B C]

   '[[A C] [B C]
     [A A C C] [A B C C] [B B C C]
     [A A A C C C] [A A B C C C]
     [A B B C C C] [B B B C C C]]
   :verbose? true
   :samples  30)

  (k-bounded-rig
   (cfg
    (:S  => :A :S+ | :B :S+)
    (:S+ => :S :C | :C)
    (:A  => A)
    (:B  => B)
    (:C  => C))

   '[A B C]

   '[[A C] [B C]
     [A A C C] [A B C C] [B B C C]
     [A A A C C C] [A A B C C C]
     [A B B C C C] [B B B C C C]]
   :verbose?    true
   :error       0.01
   :samples     30)

  ;; A^nB^mC^(n+m) loose
  (k-bounded-rig
   (cfg
    (:S => A :S+ | B :S+)
    (:S+ => :S C | C))

   '[A B C]

   '[[A C] [B C]
     [A A C C] [A B C C] [B B C C]
     [A A A C C C] [A A B C C C]
     [A B B C C C] [B B B C C C]]
   :verbose? true
   :samples  30)

  (k-bounded-rig
   (cfg
    (:S => A :S+ | B :S+)
    (:S+ => :S C | C))

   '[A B C]

   '[[A C] [B C]
     [A A C C] [A B C C] [B B C C]
     [A A A C C C] [A A B C C C]
     [A B B C C C] [B B B C C C]]
   :verbose?    true
   :error       0.01
   :samples     30)

  ;; Mathematical Expressions (loose only)
  (k-bounded-rig
   (cfg
    (:S => VAR | NUM | VAR :S1 | NUM :S1 | < :S2)
    (:S1 => + :S | * :S) (:S2 => :S >))

   '[+ * < > NUM VAR]

   '[[NUM] [VAR]
     [< NUM >] [< VAR >]
     [NUM + VAR] [VAR + NUM] [NUM + NUM] [VAR + VAR]
     [NUM * VAR] [VAR * NUM] [NUM * NUM] [VAR * VAR]
     [< NUM + VAR >]
     [NUM + NUM + NUM] [VAR + VAR + VAR]
     [NUM * VAR + NUM] [NUM * < VAR + NUM >]]
   :verbose? true
   :samples  30)

  (soft-k-bounded-rig
   (cfg
    (:S => VAR | NUM | VAR :S1 | NUM :S1 | < :S2)
    (:S1 => + :S | * :S) (:S2 => :S >))

   '[+ * < > NUM VAR]

   '[[NUM] [VAR]
     [< NUM >] [< VAR >]
     [NUM + VAR] [VAR + NUM] [NUM + NUM] [VAR + VAR]
     [NUM * VAR] [VAR * NUM] [NUM * NUM] [VAR * VAR]
     [< NUM + VAR >]
     [NUM + NUM + NUM] [VAR + VAR + VAR]
     [NUM * VAR + NUM] [NUM * < VAR + NUM >]]
   :verbose? true
   :error       0.01
   :samples     30))
