(ns cfg.logistic-regression)

(defn- sigmoid [x]
  (/ 1.0 (+ 1 (Math/exp (- x)))))

(defrecord ^:private Classifier
  [K ws])

(defn mk-classifier
  "Create an empty classifier with kernel `K`, and (optional) weight vector
  `ws` (represented as a map from samples to weights)."
  ([K ws] (->Classifier K ws))
  ([K] (mk-classifier K {})))

(defn classify
  "Given a classifier with kernel `K`, weights `ws` and offset `b`, classify
  sample point `x` using an online logistic regression classifier.

  To this end, `ws` must be a map from already seen sample points, to their
  weights.

  This function computes:
  $$
  \\sigma\\left(
    b +
    \\sum_{(x^\\prime, \\alpha) \\in ws}
    {\\alpha K(x,x^\\prime)}
  \\right)
  $$

  Where $\\sigma$ represents the sigmoid function."
  [{:keys [K ws]} x]
  (->> (for [[x* a] ws]
         (* @a (K x x*)))
       (reduce +)
       sigmoid))

(defn learn
  "Update classifier `W` with sample `x` whose true label is `y*` (0 or 1).
  When updating the classifier, a change of at most `rate` is made."
  [rate W x y*]
  (let [y (classify W x)
        d (* rate (- y* y))]
    (update-in W [:ws x] swap! + d)))
