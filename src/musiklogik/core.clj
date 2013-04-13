(ns musiklogik.core
  (:use [overtone.live])
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))
;; TODO
;; semitone, tone
;; diatonic scales: minor, major
;; chords (triads) (always with root as base to simplify?)
;; tonic, dominant, subdominant: I V IV
;; cadences

;; representation choices
;; - notes as (MIDI?)numbers
;;     semitone is step of 1, tone is 2
;; - as keywords :C, :C# etc.

(def major [2 2 1 2 2 2 1])

(def twinkle [0 0 4 4 5 5 4 3 3 2 2 1 1 0])

;; notes as map?
(def C4  { :note 0 :octave 4 }) ; (octave + 1) * 12 -> 60 midinote
(def Ds3 { :note 3 :octave 3 })

(defn semitoneo [a b] (fd/- b a 1))

(defn toneo [a b] (fd/- b a 2))

(defn multipleo [a b]
  "a is a multiple of b"
  (l/fresh [x] (fd/* b x a)))

(defn modo [a b remainder]
  (l/fresh [x y]
    (fd/in x y (fd/interval 0 100)) ;; replace with
    (fd/eq (= a (+ (* b x) remainder))
           (< remainder x))))

(defn sublisto [subl superl]
  "subl is a sublist of superl"
  (l/fresh [x y z]
    (l/appendo y z superl) ; don't
    (l/appendo x subl y))) ; reorder

(defn triado [scale root third fifth] ; really just triad in root position
  (l/fresh [second fourth]
    (sublisto [root second third fourth fifth]
              scale)))

(defn intervals->scale [tonic intervals]
  "returns a scale one longer than the nr of intervals"
  (reductions + tonic intervals))

(l/run 30 [q]
  (l/fresh [a b c]
    (triado (intervals->scale 1 major)
            a b c)
    (l/== q [a b c])))

(clojure.lang.Box. 1)


(comment
  from wikipedia:
  :perfect V I in major, V i in minor
  :plagal IV I
  :interrupted V to any other, often V7 vi in major or V7 VI in minor
  :imperfect long explanation..)
