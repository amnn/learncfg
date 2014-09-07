(ns cfg.tokenize
  (:import java.lang.IllegalArgumentException)
  )

(defn- first-matching [ms index]
  "Returns the first matcher from `ms` whose next match from `index` starts
  at `index`."
  (->> ms
       (filter (fn [[_ m]]
                 (and (.find m index)
                      (= (.start m) index))))
       first))

(defn ws [re]
  "Appends whitespace pattern to the front of a regex pattern string and
  converts it into an actual regex pattern"
  (re-pattern (str "\\s*" re)))

(defn tok
  "Convert a series of symbol, rule pairs into a function that takes a string
  and tokenizes it using the following rules:

   *  The rules are applied in the order they are given.
   *  Whitespace is ignored.
   *  The rules are defined using regular expression syntax.
   *  The rules are provided as strings themselves."

  [& rules]
  {:pre [(even? (count rules))]}
  (let [pairs (map #(update-in (vec %) [1] ws)
                   (partition 2 rules))]
    (fn tokenizer [s]
      (let [matchers (map (fn [[sym re]]
                            [sym (re-matcher re s)])
                          pairs)]
        (loop [start 0, toks []]
          (if (= start (.length s))
            toks
            (if-let [[sym m] (first-matching matchers start)]
              (recur (.end m) (conj toks sym))
              (throw (IllegalArgumentException.
                       (str "Invalid token near " start))))))))))
