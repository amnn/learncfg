(ns cfg.cfg)

(defmacro cfg [& rules])

(defmacro rule [& body])

(defn add-rule [g r])

(defn remove-rule [g r])

(defn rule-seq [g])
