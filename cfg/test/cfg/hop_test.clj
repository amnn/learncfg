(ns cfg.hop-test
  (:require [clojure.test :refer :all]
            [cfg.hop :refer :all]
            [cfg.cfg :refer [cfg]]))

(deftest hop-counts-test
  (testing "breadth first search bug"
    ;; If the inverted graph were to be traversed breadth first, from the
    ;; alpha rules up, then it is possible for a larger hop count to be
    ;; set ahead of the most efficient hop count.
    (is (= (hop-counts
             (cfg (:S  => :A :B :C :D | :E)
                  (:A  => :A1) (:A1 => a)
                  (:B  => :B1) (:B1 => a)
                  (:C  => :C1) (:C1 => a)
                  (:D  => :D1) (:D1 => a)
                  (:E  => :E1) (:E1 => :E2) (:E2 => a)))
           {:S 3,  :A  1, :B  1, :C  1, :D  1, :E  2,
            :A1 0, :B1 0, :C1 0, :D1 0, :E1 1, :E2 0})))

  (testing "re-assign queued non-terminal poorly"
    (is (= (hop-counts
            (cfg (:S => :A :B | :C)
                 (:A => a) (:B => b)
                 (:C => :A)))
           {:A 0 :B 0 :C 1 :S 1}))

    (is (= (hop-counts
            (cfg (:S => :E :D)
                 (:A => a) (:B => b)
                 (:C => :A) (:D => :A :B | :C)
                 (:E => :C)))
           {:A 0 :B 0 :C 1 :D 1 :E 2 :S 4})))

  (testing "tree like structures"
    (is (= (hop-counts
             (cfg (:S => :A :B)
                  (:A => :C :D)
                  (:B => :E :F)
                  (:C => C) (:D => D)
                  (:E => E) (:F => F)))
           {:S 3, :A 1, :B 1,
            :C 0, :D 0, :E 0, :F 0}))))
