(ns cfg.tokenize-test
  (:require [clojure.test :refer :all]
            [cfg.tokenize :refer :all])
  (:import java.lang.IllegalArgumentException))

(deftest tok-test
  (testing "even rule count"
    (is (thrown? AssertionError (tok 'A)))
    (is (thrown? AssertionError (tok 'A "a" 'B))))

  (testing "invalid tokens"
    (is (thrown? IllegalArgumentException ((tok 'A "a") "abc"))))

  (testing "tokenizing static rules"
    (is (= '[A B C] ((tok 'A "a" 'B "b" 'C "c") "abc"))))

  (testing "tokenizing dynamic rules"
    (is (= '[NUM] ((tok 'NUM "[0-9]+(\\.[0-9]+)?") "3.14"))))

  (testing "ignoring whitespace"
    (is (= '[A B C] ((tok 'A "a" 'B "b" 'C "c") " a   bc")))))
