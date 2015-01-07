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
  [graph] #(keys (get graph % {})))

(defn forest-seq
  "Given a graph as a sequence of `nodes` and a function `children`, returns
  a sequence of depth-first forests as sequences in depth-first order.
  `children` must be a function of one argument that returns a sequence of
  children that are all a member of `nodes`."
  [nodes children]

  (let [visited? (zipmap nodes (repeatedly #(atom false)))]

    (letfn [(visit! [node]
              (compare-and-set!
                (visited? node)
                false true))

            (dfs-visit [dfs-order node]
              (if (visit! node)
                (conj
                  (reduce dfs-visit
                          dfs-order
                          (children node))
                  node)
                dfs-order))]

      (reduce
        (fn [forests node]
          (if-let [dfs (seq (dfs-visit '() node))]
            (conj forests dfs)
            forests))
        '() nodes))))

(defn dfs-seq
  "Gives the depth-first ordering of nodes in a graph represented by `nodes`
  and `children` as explained in `forest-seq`."
  [nodes children] (apply concat (forest-seq nodes children)))

(defn scc
  "Returns a partition of `nodes` into the strongly connected components of the
  graph represented by `nodes` and the `l->r` edge relation.

  If a sparse adjacency list `graph` is given instead, then the nodes are
  extracted using `keys`, and the `l->r` and and `r->l` edge relations are
  created by using the `children` and `transpose` helpers."

  ([graph]
   (scc (keys graph)
        (children graph)
        (children (transpose graph))))

  ([nodes l->r r->l]
   (-> nodes
       (dfs-seq    l->r)
       (forest-seq r->l))))
