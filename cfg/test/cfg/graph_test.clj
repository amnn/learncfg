(ns cfg.graph-test
  (:require [clojure.test :refer :all]
            [cfg.graph :refer :all]))

(deftest transpose-test
  (testing "empty graph"
    (is (= {} (transpose {}))))

  (testing "single entry"
    (is (= {:b {:a 1}}
           (transpose {:a {:b 1}}))))

  (testing "2 by 2"
    (is (= {:a {:a 1 :b 3}
            :b {:a 2 :b 4}}
           (transpose {:a {:a 1 :b 2}
                       :b {:a 3 :b 4}}))))

  (testing "sparse 2 by 2"
    (is (= {:a {:a 1 :b 3}
            :b {:a 2}}
           (transpose {:a {:a 1 :b 2}
                       :b {:a 3}})))))

(deftest scc-test
  (let [no-edges {}
        ne-l->r  (children no-edges)
        ne-r->l  (children (transpose no-edges))

        ;;   a
        ;;   .
        ;;  / \
        ;; .   .
        ;; b   c

        tree   {:a {:b true :c true}}
        t-l->r (children tree)
        t-r->l (children (transpose tree))

        cyclic {:a {:b true} :b {:a true}}
        c-l->r (children cyclic)
        c-r->l (children (transpose cyclic))

        ;;    . a   e
        ;;   /  .   .
        ;;  |  / \ / \
        ;;  | .b  .d  .f
        ;;  |/     \ /
        ;;  .       .
        ;;  c       g

        merged-tree {:a {:b :d}
                     :b {:c true}
                     :c {:a true}
                     :d {:g true}
                     :e {:d true :f true}
                     :f {:g true}}
        mt-l->r (children merged-tree)
        mt-r->l (children (transpose merged-tree))]

    (letfn [(disorder [xss]
              (->> xss
                   (map #(into #{} %))
                   (into #{})))]
      (testing "empty graph"
        (is (= [] (scc [] ne-l->r ne-r->l))))

      (testing "disconnected graph"
        (is (= #{#{:a} #{:b} #{:c}}
               (disorder (scc [:a :b :c]
                              ne-l->r
                              ne-r->l)))))

      (testing "tree"
        (is (= #{#{:a} #{:b} #{:c}}
               (disorder (scc [:a :b :c]
                              t-l->r
                              t-r->l)))))

      (testing "cyclic"
        (is (= #{#{:a :b}}
               (disorder (scc [:a :b]
                              c-l->r
                              c-r->l)))))

      (testing "merged tree"
        (is (= #{#{:a :b :c} #{:d} #{:e} #{:f} #{:g}}
               (disorder (scc [:a :b :c :d :e :f :g]
                              mt-l->r mt-r->l))))))))
