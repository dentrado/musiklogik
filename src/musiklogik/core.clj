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

(def domain (fd/interval 20))

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
    (fd/in x y domain)
    (fd/eq (= a (+ (* b x) remainder))
           (< remainder b))))

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

;; All intervals series inspired by strasheela:

(defn inversional-eq-intervalo [p1 p2 interval]
  (l/fresh [x]
    (fd/in x domain)
    (fd/eq (= x (+ (- p2 p1) 12))) ; +12 to make sure it's positive
    (modo x 12 interval))
;  (fd/- p2 p1 interval)
  )

(defn all-interval-serieso [pitches intervals]
  (l/all
   (l/everyg (fn [[[p1 p2] i]]
               (inversional-eq-intervalo p1 p2 i))
             (map vector
                  (partition 2 1 pitches)
                  intervals))
   (l/distincto pitches)
   (l/distincto intervals)))

(let [pitches   (l/lvars 3)
      intervals (l/lvars (dec 3))]
  (l/run 30 [q]
    (l/== q [pitches intervals])
    (l/everyg #(fd/in % (fd/interval 0 16)) pitches)
    (l/everyg #(fd/in % (fd/interval 0 16)) intervals)
    (all-interval-serieso pitches intervals)
    ))

(l/run 10 [q]
  (l/fresh [a b c d]
    (fd/in a b c d domain)
    (fd/eq (= c (+ (- a b) 12)))
    (modo a b c)
    (l/== q [a b c])
                                        ;  (l/== q [a b c d])
    ))

(comment
  from wikipedia
  :perfect V I in major, V i in minor
  :plagal IV I
  :interrupted V to any other, often V7 vi in major or V7 VI in minor
  :imperfect long explanation..)
