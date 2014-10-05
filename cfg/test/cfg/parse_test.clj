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
