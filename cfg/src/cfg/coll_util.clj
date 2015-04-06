(ns cfg.coll-util)

(defn queue
  "Creates a queue containing the args."

  ([] clojure.lang.PersistentQueue/EMPTY)

  ([a]       (conj (queue) a))
  ([a b]     (conj (queue) a b))
  ([a b c]   (conj (queue) a b c))
  ([a b c d] (conj (queue) a b c d))

  ([a b c d & etc]
   (into (queue a b c d) etc)))

(defn concat! [xs ys]
  "Concatenates `ys` on the end of a transient *vector*, `xs`."
  [xs ys]
  (loop [ret xs, src (seq ys)]
    (if (seq src)
      (recur (conj! ret (first src))
             (rest src))
      ret)))

(defn map-v
  "Map defined over values in key-value pair collections."
  [f kvps]
  (zipmap (keys kvps)
          (->> kvps vals (map f))))

(defn map-kv
  "Map defined over keys and values in key-value pair collections.
  The fn `f` must take two arguments (a key and value), and return a pair."
  [f kvps]
  (reduce-kv (fn [coll k v]
               (apply assoc coll (f k v)))
             (empty kvps) kvps))

(defn map-kv*
  "Map defined over keys and values in key-value pair collections.
  The fn `f` takes two arguments (a key and a value), and must return the new
  value to associate with that key."
  [f kvps]
  (reduce-kv (fn [coll k v]
               (assoc coll k (f k v)))
             (empty kvps) kvps))

(defn replace-coll
  "Replace all instances of a finite sequence `ys` in a possibly infinite
  sequence `xs` with `z`."

  ([ys z]
   (fn replace* [xs]
     (lazy-seq
       (when-let [xs (seq xs)]
         (let [[hs ts] (split-at (count ys) xs)]
           (if (= hs ys)
             (cons z (replace* ts))
             (cons (first xs) (replace* (rest xs)))))))))

  ([ys z xs] ((replace-coll ys z) xs)))
