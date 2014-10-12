(ns cfg.parse-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer :all]
            [cfg.parse :refer :all]))

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
