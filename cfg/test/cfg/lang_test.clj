(ns cfg.lang-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer :all]
            [cfg.lang :refer :all]))

(deftest deriv-len-test
  (testing "left recursion"
    (let [dl (deriv-len (cfg (:S => :S A | )))]
      (are [n w] (= n (dl w))
           1 '[]
           2 '[A]
           3 '[A A])))

  (testing "no such derivation"
    (let [dl (deriv-len (cfg (:S => :S A | )))]
      (are [w] (nil? (dl w))
           '[B] '[A B] '[B A])))

  (testing "right recursion"
    (let [dl (deriv-len (cfg (:S => A :S | )))]
      (are [n w] (= n (dl w))
           1 '[]
           2 '[A]
           3 '[A A])))

  (testing "product rules"
    (let [dl (deriv-len (cfg (:S => A :S B :S | )))]
      (are [n w] (= n (dl w))
           1 '[]
           3 '[A B]
           5 '[A A B B]
           5 '[A B A B])

      (are [w] (nil? (dl w))
           '[A] '[B] '[B A] '[A A B] '[A B B] '[B A B])))

  (testing "ambiguous gramamrs"
    (let [dl (deriv-len (cfg (:S => :S :S | A :S B | )))]
      (are [n w] (= n (dl w))
           1 '[]
           2 '[A B]
           3 '[A A B B]
           5 '[A B A B])

      (are [w] (nil? (dl w))
           '[A] '[B] '[B A] '[A A B] '[A B B] '[B A B])))

  (testing "empty grammar"
    (let [dl (deriv-len (cfg (:S => )))]
      (is (= 1 (dl [])))
      (is (nil? (dl '[A]))))))

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

(deftest parse-tree-test
  (testing "left recursion"
    (let [g (cfg (:S => :A :S | A)
                 (:A => A))]
      (is (= nil (parse-tree g '[B])))

      (is (= '[[:S A] [A] []]
             (parse-tree g '[A])))

      (is (= '[[:S :A :S] [A A] [[[:A A] [A] []]
                                 [[:S A] [A] []]]]
             (parse-tree g '[A A])))))

  (testing "right recursion"
    (let [g (cfg (:S => :S :A | A)
                 (:A => A))]
      (is (= nil (parse-tree g '[B])))

      (is (= '[[:S A] [A] []]
             (parse-tree g '[A])))

      (is (= '[[:S :S :A] [A A] [[[:S A] [A] []]
                                 [[:A A] [A] []]]]
             (parse-tree g '[A A]))))))
