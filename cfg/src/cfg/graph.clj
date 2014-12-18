(ns cfg.graph
  (:require [cfg.coll-util :refer [map-v]]))

(defn transpose
  "Given the adjacency list of a directed graph, transpose its edges to
  produce the complementary graph."
  [graph]
  (reduce (partial merge-with merge) {}
          (for [[lhs adj]  graph
                [rhs edge] adj]
            {rhs {lhs edge}})))

(defn children
  "Given a sparse adjacency list of a directed `graph`, return a fn that, when
  given a node, gives the names of nodes that are reachable from it by
  following an edge in the `graph`."
  [graph] #(->> % graph keys))

(defn forest-seq
  "Given a graph as a sequence of `nodes` and a function `children`, returns
  a sequence of depth-first forests as sequences in depth-first order.
  `children` must be a function of one argument that returns a sequence of
  children that are all a member of `nodes`."
  [nodes children]

  (let [visited? (zipmap nodes (repeatedly #(atom false)))
        forests  (atom '()), dfs (atom '())]

    (letfn [(dfs-visit [n]
              (when (compare-and-set!
                      (visited? n) false true)
                (doseq [c (children n)]
                  (dfs-visit c))
                (swap! dfs conj n)))]

      (doseq [n nodes]
        (dfs-visit n)
        (when-not (empty? @dfs)
          (swap! forests conj @dfs)
          (reset! dfs '())))
      @forests)))

(defn dfs-seq
  "Gives the depth-first ordering of nodes in a graph represented by `nodes`
  and `children` as explained in `forest-seq`."
  [nodes children] (apply concat (forest-seq nodes children)))

(defn scc
  "Returns a partition of `nodes` into the strongly connected components of the
  graph represented by `nodes` and the `l->r` edge relation."
  [nodes l->r r->l]
  (-> nodes
      (dfs-seq    l->r)
      (forest-seq r->l)))
