(ns cfg.util
  (refer-clojure :exclude [interleave]))

(defn interleave
  "Returns a lazy sequence of the interleavings of sequences `xs` and `ys`
  (both potentially infinite), leaving no elements discarded."
  [xs ys]
  (lazy-seq
    (if-let [[x & xs*] (seq xs)]
      (cons x (interleave ys xs*))
      ys)))
