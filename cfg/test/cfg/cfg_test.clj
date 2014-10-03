(ns cfg.cfg-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer :all]))

(deftest cfg-test
  (testing "grammars"
    (is (= '{:S #{[:S]}}
           (cfg (:S => :S)))))

  (testing "null grammars"
    (is (= '{:S #{[]}}
           (cfg (:S => )))))

  (testing "OR rules"
    (is (= '{:S #{[A] [B C]}}
           (cfg (:S => A | B C)))))

  (testing "rule merging"
    (is (= '{:S #{[A] [B C]}}
           (cfg (:S => A)
                (:S => B C)))))

  (testing "multiple non-terminals"
    (is (= '{:S #{[A] [:B C]},
             :B #{[D E]}}
           (cfg (:S => A)
                (:B => D E)
                (:S => :B C))))))

(deftest rule-test
  (testing "rules"
    (is (= '[:S A B] (rule :S => A B))))

  (testing "null rules"
    (is (= '[:S] (rule :S => )))))

(deftest add-rule-test
  (testing "new non-terminal"
    (is (= '{:S #{[A B]}})
        (add-rule {} '[:S A B])))

  (testing "existing non-terminal"
    (is (= '{:S #{[A] [B C]}}
           (add-rule '{:S #{[A]}} '[:S B C]))))

  (testing "existing rule"
    (let [g '{:S #{[A]}}]
      (is (= g (add-rule g '[:S A]))))))

(deftest remove-rule-test
  (testing "removing a rule"
    (is (= '{:S #{[B C]}}
           (remove-rule '{:S #{[A] [B C]}}
                        '[:S A]))))

  (testing "removing a non-terminal"
    (is (= '{:S #{[A] [B C]}}
           (remove-rule '{:S #{[A] [B C]}
                          :T #{[D]}}
                        '[:T D]))))

  (testing "remove a non-existent rule"
    (let [g '{:S #{[B C]}}]
      (is (= g (remove-rule g '[:S A])))))

  (testing "removing a non-existent non-terminal"
    (let [g '{:S #{[B C]}}]
      (is (= g (remove-rule g '[:T A]))))))

(deftest rule-seq-test
  (testing "rule-seq"
    (let [rs (rule-seq '{:S #{[A] [B C]} :T #{[D]}})]
      (is (every? '#{[:S A] [:S B C] [:T D]} rs))
      (is (= 3 (count rs))))))

(deftest word?-test
  (testing "a word"
    (let [w '[A B C]]
      (is (word? w))
      (is (not (not-word? w))))))
