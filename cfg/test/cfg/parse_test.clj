(ns cfg.parse-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer :all]
            [cfg.parse :refer :all]))

(deftest nullable-test
  (testing "terminals"
    (let [g (cfg (:S => A))]
      (is (= #{} (nullable g)))))

  (testing "epsilon"
    (let [g (cfg (:S => ))]
      (is (= #{:S} (nullable g)))))

  (testing "chaining"
    (let [g (cfg (:S => :T)
                 (:T => ))]
      (is (= #{:S :T} (nullable g)))))

  (testing "branching"
    (let [g (cfg (:S => A B C | ))]
      (is (= #{:S} (nullable g)))))

  (testing "terminal rules"
    (let [g (cfg (:S => :T | :U)
                 (:T => A B C)
                 (:U => D | ))]
      (is (= #{:S :U} (nullable g)))))

  (testing "self reference"
    (let [g (cfg (:S => :S))]
      (is (= #{} (nullable g)))))

  (testing "duplicate symbols"
    (let [g (cfg (:S => :A :A)
                 (:A => ))]
      (is (= #{:A :S} (nullable g)))))

  (testing "multiple dependencies"
    (let [g (cfg (:S => :A :B)
                 (:A => A | )
                 (:B => B | ))]
      (is (= #{:A :B :S} (nullable g)))))

  (testing "cyclic dependencies"
    (let [g (cfg (:S => :T)
                 (:T => :S | ))]
      (is (= #{:S :T} (nullable g))))

    (let [g (cfg (:S => :T)
                 (:T => :U | :V)
                 (:U => )
                 (:V => :T))]
      (is (= #{:U :T :S :V} (nullable g))))))

(deftest recogniser-test
  (testing "left recursion"
    (let [in-lang? (recogniser (cfg (:S => :S A | )))]
      (are [w] (in-lang? w)
           [] '[A] '[A A] '[A A A])

      (are [w] (not (in-lang? w))
           '[B] '[A B] '[B A])))

  (testing "right recursion"
    (let [in-lang? (recogniser (cfg (:S => A :S | )))]
      (are [w] (in-lang? w)
           [] '[A] '[A A] '[A A A])
      (are [w] (not (in-lang? w))
           '[B] '[A B] '[B A])))

  (testing "product rules"
    (let [in-lang? (recogniser (cfg (:S => A :S B :S | )))]
      (are [w] (in-lang? w)
           [] '[A B] '[A A B B] '[A B A B])

      (are [w] (not (in-lang? w))
           '[A] '[B] '[B A] '[A A B] '[A B B] '[B A B])))

  (testing "ambiguous grammar"
    (let [in-lang? (recogniser (cfg (:S => :S :S |  A :S B | )))]
      (are [w] (in-lang? w)
           [] '[A B] '[A A B B] '[A B A B])
      (are [w] (not (in-lang? w))
           '[A] '[B] '[B A] '[A A B] '[A B B] '[B A B])))

  (testing "empty grammar"
    (let [in-lang? (recogniser (cfg (:S => )))]
      (is (in-lang? []))
      (is (not (in-lang? '[A]))))))
