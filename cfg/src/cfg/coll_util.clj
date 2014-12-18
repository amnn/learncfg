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

(defn map-v
  "Map defined over values in key-value pair collections."
  [f kvps]
  (zipmap (keys kvps)
          (->> kvps vals (map f))))

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

(defn transpose
  "Given a two-level nested map, swap the order of nesting."
  [xss]
  (reduce (partial merge-with merge)
          (for [[sym xs] xss
                [k   v]  xs]
            {k {sym v}})))
