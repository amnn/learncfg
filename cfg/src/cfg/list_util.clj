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


