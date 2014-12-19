(ns cfg.coll-util-test
  (:require [clojure.test :refer :all]
            [cfg.coll-util :refer :all]))

(deftest map-v-test
  (testing "empty map"
    (is (= {} (map-v identity {}))))

  (testing "identity map"
    (let [m {:a :b, :c :d}]
      (is (= m (map-v identity m)))))

  (testing "applying the function"
    (is (= {:a 1, :b 2}
           (map-v inc {:a 0, :b 1})))))

(deftest map-kv-test
  (testing "empty map"
    (is (= {} (map-kv vector {}))))

  (testing "identity map"
    (is (= {:a :b, :c :d}
           (map-kv vector {:a :b, :c :d}))))

  (testing "applying the function"
    (is (= {1 2, 3 4}
           (map-kv (fn [k v]
                     [(inc k) (inc v)])
                   {0 1, 2 3}))))

  (testing "duplicated keys"
    (is (= {:a 1}
           (map-kv (fn [k v] [:a 1])
                   {:a 1, :b 2, :c 3})))))


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
