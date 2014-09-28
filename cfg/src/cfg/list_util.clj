(ns cfg.list-util)

(defn queue
  "Creates a queue containing the ars."

  ([] clojure.lang.PersistentQueue/EMPTY)
  ([a] (conj clojure.lang.PersistentQueue/EMPTY a))
  ([a b] (conj clojure.lang.PersistentQueue/EMPTY a b))
  ([a b c] (conj clojure.lang.PersistentQueue/EMPTY a b c))
  ([a b c d] (conj clojure.lang.PersistentQueue/EMPTY a b c d))

  ([a b c d & etc]
   (into clojure.lang.PersistentQueue/EMPTY (conj etc d c b a))))

(defn combine
  "Takes a finite sequence of finite sequences and produces the sequence of
  the cartesian product of those sequences."
  [xss]
  (if-let [xss* (seq xss)]
    (for [x (first xss*)
          cs (combine (rest xss*))]
      (cons x cs))
    '(())))

(defn bfs-seq
  "Takes a fn `children` that, given a node in the tree, returns its children,
  or nil if there are none, and a root element. Performs a lazy breadth first
  search."
  [children root]
  (letfn [(step [q]
            (lazy-seq
              (when (seq q)
                (let [n (peek q)]
                  (cons
                    n (-> (pop q)
                          (into (children n))
                          step))))))]
    (step (queue root))))
