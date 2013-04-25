(ns musiklogik.all-interval-series
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(def domain (fd/interval 24))

(defn modo [a b remainder]
  (fresh [x y]
    (fd/in x y domain)
    (fd/eq (= a (+ (* b x) remainder))
           (< remainder b))))

(defn sublisto "subl is a sublist of superl"
  [subl superl]
  (fresh [x y z]
    (appendo y z superl) ; don't
    (appendo x subl y))) ; reorder


(defn triado [scale root third fifth] ; really just triad in root position
  (fresh [second fourth]
    (sublisto [root second third fourth fifth]
              scale)))

(defn intervals->scale
  "returns a scale one longer than the nr of intervals"
  [tonic intervals]
  (reductions + tonic intervals))

;; All intervals series inspired by strasheela:

(defn inversional-eq-intervalo [p1 p2 interval]
  (fresh [x]
    (fd/in x domain)
    (fd/eq (= x (+ (- p2 p1) 12))) ; +12 to make sure it's positive
    (modo x 12 interval))
;  (fd/- p2 p1 interval)
  )

(defn all-interval-serieso [pitches intervals]
  (all
   (fd/distinct pitches)
   (fd/distinct intervals)
   (everyg (fn [[[p1 p2] i]]
               (inversional-eq-intervalo p1 p2 i))
             (map vector
                  (partition 2 1 pitches)
                  intervals))))

(def all-intervals-series (let [len 12
                                pitches   (lvars len)
                                intervals (lvars (dec len))]
                            (run 2 [q]
                              (== q [pitches intervals])
                              (everyg #(fd/in % (fd/interval 12)) pitches)
                              (everyg #(fd/in % (fd/interval 12)) intervals)
                              (all-interval-serieso pitches intervals))))
