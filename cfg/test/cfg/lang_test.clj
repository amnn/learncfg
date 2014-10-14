(ns cfg.lang-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer :all]
            [cfg.lang :refer :all]))

(deftest in-lang-test
  (testing "left recursion"
    (let [in-lang? (in-lang (cfg (:S => :S A | )))]
      (are [w] (in-lang? w)
           [] '[A] '[A A] '[A A A])

      (are [w] (not (in-lang? w))
           '[B] '[A B] '[B A])))

  (testing "right recursion"
    (let [in-lang? (in-lang (cfg (:S => A :S | )))]
      (are [w] (in-lang? w)
           [] '[A] '[A A] '[A A A])
      (are [w] (not (in-lang? w))
           '[B] '[A B] '[B A])))

  (testing "product rules"
    (let [in-lang? (in-lang (cfg (:S => A :S B :S | )))]
      (are [w] (in-lang? w)
           [] '[A B] '[A A B B] '[A B A B])

      (are [w] (not (in-lang? w))
           '[A] '[B] '[B A] '[A A B] '[A B B] '[B A B])))

  (testing "ambiguous grammar"
    (let [in-lang? (in-lang (cfg (:S => :S :S |  A :S B | )))]
      (are [w] (in-lang? w)
           [] '[A B] '[A A B B] '[A B A B])
      (are [w] (not (in-lang? w))
           '[A] '[B] '[B A] '[A A B] '[A B B] '[B A B])))

  (testing "empty grammar"
    (let [in-lang? (in-lang (cfg (:S => )))]
      (is (in-lang? []))
      (is (not (in-lang? '[A]))))))

(deftest lang-seq-test
  (testing "left recursion"
    (let [g (cfg (:S => :S A | ))]
      (is (= '([] [A] [A A] [A A A] [A A A A])
             (take 5 (lang-seq g))))))

  (testing "right recursion"
    (let [g (cfg (:S => A :S | ))]
      (is (= '([] [A] [A A] [A A A] [A A A A])
             (take 5 (lang-seq g))))))

  (testing "product rules"
    (let [ws (take 4 (lang-seq (cfg (:S => A :S B :S | ))))]
      (is (every? '#{[] [A B] [A B A B] [A A B B]} ws))
      (is (= 4 (count (into #{} ws))))))

  (testing "ambiguous grammar"
    (let [ws (take 4 (lang-seq (cfg (:S => :S :S | A :S B | ))))]
      (is (every? '#{[] [A B] [A B A B] [A A B B]} ws))
      (is (= 4 (count (into #{} ws)))))))
