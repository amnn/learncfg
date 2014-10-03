(ns cfg.parse-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer :all]
            [cfg.parse :refer :all]))


(deftest erasable-test
  (testing "terminals"
    (let [g (cfg (:S => A))]
      (is (= #{} (erasable g)))))

  (testing "epsilon"
    (let [g (cfg (:S => ))]
      (is (= #{:S} (erasable g)))))

  (testing "chaining"
    (let [g (cfg (:S => :T)
                 (:T => ))]
      (is (= #{:S :T} (erasable g)))))

  (testing "branching"
    (let [g (cfg (:S => A B C | ))]
      (is (= #{:S} (erasable g)))))

  (testing "terminal rules"
    (let [g (cfg (:S => :T | :U)
                 (:T => A B C)
                 (:U => D | ))]
      (is (= #{:S :U} (erasable g)))))

  (testing "self reference"
    (let [g (cfg (:S => :S))]
      (is (= #{:S} (erasable g)))))

  (testing "cyclic dependencies"
    (let [g (cfg (:S => :T)
                 (:T => :S | ))]
      (is (= #{:S :T} (erasable g))))

    (let [g (cfg (:S => :T)
                 (:T => :U | :V)
                 (:U => )
                 (:V => :T))]
      (is (= #{:U :T :S :V} (erasable g))))))
