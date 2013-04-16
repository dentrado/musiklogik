(ns musiklogik.core-test
  (:use clojure.test
        musiklogik.core)
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(deftest modo-test
  (testing "modo test"
    (is (= [1]
           (l/run* [q]
             (fd/in q (fd/interval 0 10))
             (modo 5 2 q))))
    (is (= [3]
           (l/run* [q]
             (fd/in q (fd/interval 0 10))
             (modo 3 12 q))))))
