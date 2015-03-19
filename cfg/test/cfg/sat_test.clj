(ns cfg.sat-test
  (:require [clojure.test :refer :all]
            [cfg.sat :refer :all]))

(deftest horn-sat-test
  (testing "true -> S"
    (is (= #{:S} (horn-sat [[:S]]))))

  (testing "true -> T & T -> S"
    (is (= #{:S :T} (horn-sat [[:S :T] [:T]]))))

  (testing "true -> V & V -> U & U -> T & T -> S"
    (is (= #{:S :T :U :V}
           (horn-sat [[:S :T] [:T :U] [:U :V] [:V]]))))

  (testing "true -> T & true -> U & (T & U -> S)"
    (is (= #{:S :T :U}
           (horn-sat [[:S :T :U] [:T] [:U]]))))

  (testing "true -> T & (T & U -> S)"
    (is (= #{:T} (horn-sat [[:S :T :U] [:T]]))))

  (testing "true -> T & (T & T -> S)"
    (is (= #{:S :T}
           (horn-sat [[:T] [:S :T :T]]))))

  (testing "true -> T & (T -> S) & (S & T -> S) & (S & U -> U)"
    (is (= #{:S :T}
           (horn-sat [[:T] [:S :T] [:S :S :T] [:U :S :U]])))))
