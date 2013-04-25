(ns musiklogik.core
  (:use leipzig.melody
        leipzig.scale
        leipzig.canon
        leipzig.live)
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [overtone.live :as overtone]
            [overtone.synth.stringed :as strings]))

;; Leipzig example copypasta
(strings/gen-stringed-synth ektara 1 true)

(defn pick [distort amp {midi :pitch, start :time, length :duration}]
    (let [synth-id (overtone/at start
                     (ektara midi :distort distort :amp amp :gate 1))]
      (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

(defmethod play-note :leader [note]
  (pick 0.7 1.0 note))
(defmethod play-note :follower [note]
  (pick 0.3 1.0 note))
(defmethod play-note :bass [note]
  (pick 0.9 0.2 (update-in note [:pitch] #(- % 12))))
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def domain (fd/interval 24))

(defn play-equal [pitches]
  (let [speed (bpm 120)]
    (->> (phrase (repeat 1) pitches)
         (where :part (is :leader))
         (where :time speed)
         (where :duration speed)
         (where :pitch (comp C major))
         (play))))



(comment
  from wikipedia
  :perfect V I in major, V i in minor
  :plagal IV I
  :interrupted V to any other, often V7 vi in major or V7 VI in minor
  :imperfect long explanation..)

(comment
  (l/run 30 [q]
    (l/fresh [a b c]
      (triado (intervals->scale 1 [2 2 1 2 2 2 1])
              a b c)
      (l/== q [a b c]))))
