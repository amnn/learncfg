(ns cfg.learn.simple-test
  (:require [clojure.test :refer :all]
            [cfg.cfg :refer [cfg]]
            [cfg.learn.simple :refer :all]))

(deftest merge-nts-test
  (testing "merge-nts"
    (let [g (cfg (:S => A | B) (:T => C))]
      (is (= (merge-nts g :S :T)
             (cfg (:S => A | B | C))))
      (is (= (merge-nts g :T :S)
             (cfg (:T => A | B | C))))))

  (testing "merge-nts unrelated rules"
    (let [g (cfg (:S => A) (:T => B) (:U => C))]
      (is (= (merge-nts g :S :T)
             (cfg (:S => A | B) (:U => C)))))))
