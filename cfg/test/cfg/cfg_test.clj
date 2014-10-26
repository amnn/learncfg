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

(deftest remove-nt-test
  (let [g '{:S #{[A] [ C]} :T #{[D]}}]
    (testing "remove-nt"
      (is (= (remove-nt g :S)
             '{:T #{[D]}})))

    (testing "remove-nt non-existent"
      (is (= (remove-nt g :A) g)))))

(deftest rule-seq-test
  (let [g '{:S #{[A] [B C]} :T #{[D]}}
        rs (rule-seq g)]
    (testing "rule-seq"
      (is (every? '#{[:S A] [:S B C] [:T D]} rs))
      (is (= 3 (count rs))))

    (testing "filtered rule-seq"
      (is (= '([:T D]) (rule-seq g :T))))))

(deftest mapr-test
  (let [g '{:S #{[A] [B C]} :T #{[D]}}]
    (testing "mapr identity"
      (is (= (mapr identity g) g)))

    (testing "mapr const"
      (is (= '{:S #{[A]}}
             (mapr (fn [rs] (vector :S 'A)) g))))

    (testing "mapr preserve non-terminals"
      (is (= '{:S #{[A]} :T #{[A]}}
             (mapr (fn [[s & rs]] [s 'A]) g))))))

(deftest mapr*-test
  (let [g '{:S #{[A] [B C]} :T #{[D]}}]
    (testing "mapr* identity"
      (is (= (mapr* identity g) g)))

    (testing "mapr* const"
      (is (= '{:S #{[A]} :T #{[A]}}
             (mapr* (fn [rs] (vector 'A)) g))))))

(deftest word?-test
  (testing "a word"
    (let [w '[A B C]]
      (is (word? w))
      (is (not (not-word? w))))))

(deftest terminal?-test
  (testing "terminals"
    (is (terminal? 'A))
    (is (not (terminal? :A)))))

(deftest non-terminal?-test
  (testing "non-terminals"
    (is (non-terminal? :A))
    (is (not (non-terminal? 'A)))))
