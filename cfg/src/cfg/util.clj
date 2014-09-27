(ns cfg.util
  (refer-clojure :exclude [interleave]))

(defn interleave
  "Returns a lazy sequence of the interleavings of sequences `xs` and `ys`
  (both potentially infinite), leaving no elements discarded."
  [xs ys]
  (lazy-seq
    (if-let [xs* (seq xs)]
      (cons (first xs*) (interleave ys (rest xs*)))
      ys)))

(defn interleave*
  "Converts a sequence of potentially infinite sequences into its lazy
  interleaving."
  [xss]
  (lazy-seq
    (when-let [xss* (seq xss)]
      (interleave (first xss*)
                  (interleave* (rest xss*))))))

(defn combine
  "Takes a finite sequence of potentially infinite sequences, and combines
  them to produce a possibly infinite sequence of their cartesian product."
  [xss]
  (if-let [xss* (seq xss)]
    (interleave*
      (for [x (first xss*)]
        (for [cs (combine (rest xss*))]
          (lazy-seq (cons x cs)))))
    '(())))
