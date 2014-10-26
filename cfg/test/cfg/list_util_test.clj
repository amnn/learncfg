(ns cfg.list-util-test
  (:require [clojure.test :refer :all]
            [cfg.list-util :refer :all]))

(deftest replace-coll-test
  (testing "replace-coll"
    (is (= (replace-coll [2 3] \r [1 2 3])
           [1 \r])))

  (testing "replace-coll multiple instances"
    (is (= [1 \r 4 \r]
           (replace-coll [2 3] \r [1 2 3 4 2 3]))))

  (testing "replace-coll no instances"
    (let [xs [4 5 6 7 8 9]]
      (is (= xs (replace-coll [2 3] \r xs)))))

  (testing "replace-coll partial instances"
    (let [xs [1 2 4 5 2 6]]
      (is (= xs (replace-coll [2 3] \r xs)))))

  (testing "replace-coll adjacent instances"
    (is (= [\r \r] (replace-coll [2 3] \r [2 3 2 3])))))
