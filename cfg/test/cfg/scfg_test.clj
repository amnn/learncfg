(ns cfg.scfg-test
  (:require [clojure.test :refer :all]
            [cfg.scfg :refer :all]
            [clojure.core.matrix.operators :as m]
            [cfg.cfg :refer [cfg]]))

(deftest cfg->scfg-test
  (testing "cfg->scfg"
    (let [sg (cfg->scfg (cfg (:S => A) (:T => B | C)))]
      (are [r p] (m/== (get-in sg r 0) p)
           [:S '[A]] 1
           [:T '[B]] 1/2
           [:T '[C]] 1/2))))

(deftest e-graph-test
  (testing "e-graph"
    (let [sg '{:S {[:S :S] 1/3
                   [:A :B] 1/3
                   [C]     1/3}
               :A {[A]     1/2
                   [D]     1/2}
               :B {[B]     1}}
          graph (e-graph sg)
          T     :cfg.scfg/T]
      (are [r c p] (== p (get-in graph [r c] 0))
           :S :S 2/3, :S :A 1/3, :S :B 1/3, :S T 1/3
           :A :S 0,   :A :A 0,   :A :B 0,   :A T 1
           :B :S 0,   :B :A 0,   :B :B 0,   :B T 1)) ))

(deftest e-system-test
  (testing "e-system"
    (let [sg '{:S {[:S :S] 1/3
                   [:A :B] 1/3
                   [C]     1/3}
               :A {[A]     1/2
                   [D]     1/2}
               :B {[B]     1}}

          {:keys [order M v]} (e-system sg)

          unordered-v {:S 1/3, :A 1, :B 1}
          sparse-m    {:S {:S 2/3, :A 1/3, :B 1/3}}]
      (is (m/== v (for [i order] (get unordered-v i 0))))
      (is (m/== M (for [i order]
                    (for [j order]
                      (get-in sparse-m [i j] 0))))))))

(deftest strongly-consistent?-test
  (testing "finite grammar"
    (is (->> (cfg (:S => :A :B)
                  (:A => :B :C)
                  (:A => A)
                  (:B => B)
                  (:C => C))
             cfg->scfg
             strongly-consistent?)))

  (testing "infinite grammar"
    (is (not (->> (cfg (:S => :L :R)
                       (:L => :L :S | L)
                       (:R => :R :S | R))
                  cfg->scfg
                  strongly-consistent?)))

    (is (strongly-consistent?
          '{:S {[:L :R] 1}
            :L {[:L :S] 1/3 [L] 2/3}
            :R {[:R :S] 1/3 [R] 2/3}}))))
