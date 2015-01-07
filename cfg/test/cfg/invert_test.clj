(ns cfg.invert-test
  (:require [clojure.test :refer :all]
            [cfg.invert :refer :all]))

(deftest invert-graph-test
  (testing "terminal rules"
    (let [ig (invert-graph
               '([:S A] [:T B]))]
      (is (= (get ig nil)
             #{:S :T}))))

  (testing "non-terminal rules"
    (let [ig (invert-graph
               '([:S :S] [:T :T]))]
      (is (= (->> (get ig :S)
                  (map deref)
                  (into #{}))
             #{#cfg.invert.Rule[1 :S [:S]]}))

      (is (= (->> (get ig :T)
                  (map deref)
                  (into #{}))
             #{#cfg.invert.Rule[1 :T [:T]]}))))

  (testing "shared rule"
    (let [ig (invert-graph
               '([:S :A :B]))]
      (is (= (get ig :A)
             (get ig :B))))))
