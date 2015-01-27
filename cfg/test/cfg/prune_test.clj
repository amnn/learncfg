(ns cfg.prune-test
  (:require [clojure.test :refer :all]
            [cfg.prune :refer :all]))

(deftest reachable-nts-test
  (testing "everything reachable"
    (is (= #{:S :T} (reachable-nts
                      '{:S #{[:T]} :T #{[]}}))))

  (testing "nothing reachable"
    (is (= #{:S} (reachable-nts
                   '{:S #{[]} :T #{[]}}))))

  (testing "multiple roots"
    (is (= #{:S :T} (reachable-nts
                      '{:S #{[:T]}
                        :T #{[:S]}
                        :B #{[:T]}})))))

(deftest contributing-nts-test
  (testing "empty grammar"
    (is (= #{} (contributing-nts '{}))))

  (testing "all terminal rules"
    (is (= #{:S :T}
           (contributing-nts
             '{:S #{[A]}
               :T #{[B]}}))))

  (testing "empty rules"
    (is (= #{:S} (contributing-nts '{:S #{[]}}))))

  (testing "mix of terminal and non-terminal rules"
    (is (= #{:S :T}
           (contributing-nts
             '{:S #{[A] [:B]}
               :T #{[B] [:B]}}))))

  (testing "mixing terminals and non-terminals in rules"
    (is (= #{:S}
           (contributing-nts
             '{:S #{[A] [:B]}
               :T #{[A :B]}})))))

(deftest prune-cfg-test
  (testing "no superfluous grammars"
    (let [g '{:S #{[A]}}]
      (is (= g (prune-cfg g)))))

  (testing "unreachable rules"
    (is (= '{:S #{[A :T]}
             :T #{[]}}
           (prune-cfg '{:S #{[A :T]}
                        :T #{[]}
                        :B #{[A B]}}))))

  (testing "non-contributing rules"
    (is (= '{:S #{[A]}}
           (prune-cfg '{:S #{[A] [A :T]}
                        :T #{[:T]}}))))

  (testing "non-contributing rules that when removed, create unreachable rules"
    (is (= '{:S #{[A]}}
           (prune-cfg '{:S #{[A] [A :T]}
                        :T #{[:T :U]}
                        :U #{[B]}})))))

(deftest prune-scfg-test
  (testing "threshold probability"
    (is (= (prune-scfg 0.5 '{:S {[A B] 0.6
                                 [C D] 0.4}})
           '{:S {[A B] 0.6}})))

  (testing "non-reachable rules"
    (is (= (prune-scfg 0.5 '{:S {[A :T] 0.4
                                 [C D] 0.6}
                             :T {[E F] 1.0}})
           '{:S {[C D] 0.6}})))

  (testing "non-contributing rules"
    (is (= (prune-scfg 0.4 '{:S {[A :T] 0.5
                                 [C D]  0.5}
                             :T {[E F] 0.3
                                 [:A :B] 0.7}})
           '{:S {[C D] 0.5}}))))
