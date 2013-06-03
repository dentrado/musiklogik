(ns musiklogik.core-test
  (:use clojure.test
        musiklogik.core)
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [leipzig.melody :as melody]))

(comment
  (deftest modo-test
    (testing "modo test"
      (is (= [1]
             (l/run* [q]
               (fd/in q (fd/interval 0 10))
               (modo 5 2 q))))
      (is (= [3]
             (l/run* [q]
               (fd/in q (fd/interval 0 10))
               (modo 3 12 q)))))))

(def tewinkle (melody/phrase
              [1 1 1 1, 1 1 2, 1 1 1 1, 1 1 2]
              [0 0 4 4, 5 5 4, 3 3 2 2, 1 1 0]))

(deftest bars-test
  (testing "bars test"
    (is (= (bars tewinkle)
         [[{:duration 1, :pitch 0, :time 0}
           {:duration 1, :pitch 0, :time 1}
           {:duration 1, :pitch 4, :time 2}
           {:duration 1, :pitch 4, :time 3}]
          [{:duration 1, :pitch 5, :time 4}
           {:duration 1, :pitch 5, :time 5}
           {:duration 2, :pitch 4, :time 6}]
          [{:duration 1, :pitch 3, :time 8}
           {:duration 1, :pitch 3, :time 9}
           {:duration 1, :pitch 2, :time 10}
           {:duration 1, :pitch 2, :time 11}]
          [{:duration 1, :pitch 1, :time 12}
           {:duration 1, :pitch 1, :time 13}
           {:duration 2, :pitch 0, :time 14}]]))))

(deftest passing-toneo-test
  (testing "Passing tone test"
    (is (= [{:pitch 2, :prev {:pitch 1}, :next {:pitch 3}}
            {:pitch 9, :prev {:pitch 1}, :next {:pitch 3}}]
           (l/run* [q]
             (l/fresh [a b c]
               (l/== a { :pitch 1 })
               (l/== b { :pitch 3 })
               (l/== q { :pitch c :prev a :next b})
               (passing-noteo q)))))))

(deftest triado-test
  (testing "triado test"
    (is (= [[1 3 5]]
         (l/run 1 [q]
           (l/fresh [a b]
             (l/== q [1 a b])
             (triado q)))))))
