(ns musiklogik.send-more-money
  (:refer-clojure :exclude [== distinct])
  (:use [clojure.core.logic :only [run* fresh ==]]
        [clojure.core.logic.fd :only [in distinct != eq interval]]))

(run* [q]
  (fresh [S E N D M O R Y]
    (in S E N D M O R Y (interval 0 9))
    (distinct [S E N D M O R Y])
    (!= S 0)
    (!= M 0)
    (eq (= (+          (+ (* 1000 S) (* 100 E) (* 10 N) D)
                       (+ (* 1000 M) (* 100 O) (* 10 R) E))
           (+ (* 10000 M) (* 1000 O) (* 100 N) (* 10 E) Y)))
    (== q [S E N D M O R Y])))
