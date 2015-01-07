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

(deftest prune-test
  (testing "no superfluous grammars"
    (let [g '{:S #{[A]}}]
      (is (= g (prune g)))))

  (testing "unreachable rules"
    (is (= '{:S #{[A :T]}
             :T #{[]}}
           (prune '{:S #{[A :T]}
                    :T #{[]}
                    :B #{[A B]}}))))

  (testing "non-contributing rules"
    (is (= '{:S #{[A]}}
           (prune '{:S #{[A] [A :T]}
                    :T #{[:T]}}))))

  (testing "non-contributing rules that when removed, create unreachable rules"
    (is (= '{:S #{[A]}}
           (prune '{:S #{[A] [A :T]}
                    :T #{[:T :U]}
                    :U #{[B]}})))))

