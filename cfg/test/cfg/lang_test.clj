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
      (is (= 4 (count (into #{} ws))))))

  (testing "finite grammars"
    (is (= '[[A] [B C] [D E F]]
           (lang-seq (cfg (:S => A | B C | D E F)))))))

(deftest parse-trees-test
  (testing "loosened CNF form"
    (let [g (cfg (:S => A :S | A))]
      (is (= nil (parse-trees g '[B])))

      (is (= #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
             (parse-trees g '[A])))

      (is (= #cfg.lang.MultiBranch
             [:S [A A]
              #{[[:S A :S]
                 #cfg.lang.Terminal[A]
                 #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]]}]
             (parse-trees g '[A A])))))

  (testing "left recursion"
    (let [g (cfg (:S => :A :S | A)
                 (:A => A))]
      (is (= nil (parse-trees g '[B])))

      (is (= #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
             (parse-trees g '[A])))

      (is (= #cfg.lang.MultiBranch
             [:S [A A]
              #{[[:S :A :S]
                 #cfg.lang.MultiLeaf[:A [A] #{[[:A A]]}]
                 #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]]}]
             (parse-trees g '[A A])))))

  (testing "right recursion"
    (let [g (cfg (:S => :S :A | A)
                 (:A => A))]
      (is (= nil (parse-trees g '[B])))

      (is (= #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
             (parse-trees g '[A])))

      (is (= #cfg.lang.MultiBranch
             [:S [A A]
              #{[[:S :S :A]
                 #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
                 #cfg.lang.MultiLeaf[:A [A] #{[[:A A]]}]]}]
             (parse-trees g '[A A])))))

  (testing "ambiguous grammar"
    (let [g (cfg (:S => :S :S | A))]
      (is (= #cfg.lang.MultiBranch
             [:S [A A A]
              #{[[:S :S :S]
                 #cfg.lang.MultiBranch
                 [:S [A A]
                  #{[[:S :S :S]
                     #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
                     #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]]}]
                 #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]]

                [[:S :S :S]
                 #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
                 #cfg.lang.MultiBranch
                 [:S [A A]
                  #{[[:S :S :S]
                     #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]
                     #cfg.lang.MultiLeaf[:S [A] #{[[:S A]]}]]}]]}]
             (parse-trees g '[A A A]))))))

(deftest ml-tree-test
  (testing "terminals"
    (is (= #cfg.lang.PLeaf[:S [:S A] 1.0 [A]]
           (ml-tree '{:S {[A] 1.0}} '[A]))))

  (testing "unambiguous grammar"
    (is (= #cfg.lang.PBranch
           [:S [:S :A :S] 0.25 [A A]
            #cfg.lang.PLeaf[:A [:A A] 1.0 [A]]
            #cfg.lang.PLeaf[:S [:S A] 0.5 [A]]]
           (ml-tree '{:S {[:A :S] 0.5
                          [A]     0.5}
                      :A {[A]     1.0}}
                    '[A A]))))

  (testing "ambiguous grammar"
    (is (= #cfg.lang.PBranch
           [:S [:S :A :S] 0.0625 [A A]
            #cfg.lang.PLeaf[:A [:A A] 1.0  [A]]
            #cfg.lang.PLeaf[:S [:S A] 0.25 [A]]]
           (ml-tree '{:S {[:A :S] 0.25
                          [:S :S] 0.5
                          [A]     0.25}
                      :A {[A]     1.0}}
                    '[A A])))))

(deftest ml-p-test
  (testing "non-existent parse"
    (is (= 0.0 (ml-p '{:S {[A] 1.0}} '[B]))))

  (testing "terminals"
    (is (= 1.0 (ml-p '{:S {[A] 1.0}} '[A]))))

  (testing "unambiguous grammar"
    (is (= 0.25
           (ml-p '{:S {[:A :S] 0.5
                       [A]     0.5}
                   :A {[A]     1.0}}
                 '[A A]))))

  (testing "ambiguous grammar"
    (is (= 0.0625
           (ml-p '{:S {[:A :S] 0.25
                       [:S :S] 0.5
                       [A]     0.25}
                   :A {[A]     1.0}}
                 '[A A])))))

(deftest inside-p-tree
  (testing "terminals"
    (is (= #cfg.lang.MultiPLeaf[:S [A] 1.0 #{[[:S A]]}]
           (inside-p '{:S {[A] 1.0}} '[A]))))

  (testing "unambiguous grammar"
    (is (= #cfg.lang.MultiPBranch
           [:S [A A] 0.25
            #{[[:S :A :S]
               #cfg.lang.MultiPLeaf[:A [A] 1.0 #{[[:A A]]}]
               #cfg.lang.MultiPLeaf[:S [A] 0.5 #{[[:S A]]}]]}]
           (inside-p '{:S {[:A :S] 0.5
                           [A]     0.5}
                       :A {[A]     1.0}}
                     '[A A]))))

  (testing "ambiguous grammar"
    (is (= #cfg.lang.MultiPBranch
           [:S [A A] 0.09375
            #{[[:S :A :S]
               #cfg.lang.MultiPLeaf[:A [A] 1.0  #{[[:A A]]}]
               #cfg.lang.MultiPLeaf[:S [A] 0.25 #{[[:S A]]}]]
              [[:S :S :S]
               #cfg.lang.MultiPLeaf[:S [A] 0.25 #{[[:S A]]}]
               #cfg.lang.MultiPLeaf[:S [A] 0.25 #{[[:S A]]}]]}]
           (inside-p '{:S {[:A :S] 0.25
                           [:S :S] 0.5
                           [A]     0.25}
                       :A {[A]     1.0}}
                     '[A A])))))
