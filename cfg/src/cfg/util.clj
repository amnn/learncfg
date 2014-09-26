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

(defn interleave*
  "Takes a (potentially infinite) sequence of (potentially infinite) sequences
  and returns the interleavings of all of them."
  [xss]
  (lazy-seq
    (when-let [[xs & xss*] (seq xss)]
      (interleave xs (interleave* xss*)))))
