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
;(def domain (fd/domain 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))

;(def major [2 2 1 2 2 2 1])

;; notes as map?
(def C4  { :note 0 :octave 4 }) ; (octave + 1) * 12 -> 60 midinote
(def Ds3 { :note 3 :octave 3 })

(defn semitoneo [a b] (fd/- b a 1))

(defn toneo [a b] (fd/- b a 2))

(defn multipleo "a is a multiple of b"
  [a b]
  (l/fresh [x] (fd/* b x a)))

(defn modo [a b remainder]
  (l/fresh [x y]
    (fd/in x y domain)
    (fd/eq (= a (+ (* b x) remainder))
           (< remainder b))))

(defn sublisto "subl is a sublist of superl"
  [subl superl]
  (l/fresh [x y z]
    (l/appendo y z superl) ; don't
    (l/appendo x subl y))) ; reorder

(defn triado [scale root third fifth] ; really just triad in root position
  (l/fresh [second fourth]
    (sublisto [root second third fourth fifth]
              scale)))

(defn intervals->scale
  "returns a scale one longer than the nr of intervals"
  [tonic intervals]
  (reductions + tonic intervals))

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
   (fd/distinct pitches)
   (fd/distinct intervals)
   (l/everyg (fn [[[p1 p2] i]]
               (inversional-eq-intervalo p1 p2 i))
             (map vector
                  (partition 2 1 pitches)
                  intervals))))

(defn play-equal [pitches]
  (let [speed (bpm 120)]
    (->> (phrase (repeat 1) pitches)
         (where :part (is :leader))
         (where :time speed)
         (where :duration speed)
         (where :pitch (comp C major))
         (play))))

(def all-intervals-series (let [len 12
                                pitches   (l/lvars len)
                                intervals (l/lvars (dec len))]
                            (l/run 2 [q]
                              (l/== q [pitches intervals])
                              (l/everyg #(fd/in % (fd/interval 12)) pitches)
                              (l/everyg #(fd/in % (fd/interval 12)) intervals)
                              (all-interval-serieso pitches intervals))))

(def twinkle [0 0 4 4 5 5 4 4 3 3 2 2 1 1 0 0])

(def fandbs
  (let [speed (bpm 120)
        len 16
        follower (l/lvars len)
        bass     (l/lvars len)
        [[f b] :as all] (l/run 2 [q]
                          (l/== q [follower bass])
                          (l/everyg #(fd/in % (fd/interval 12)) follower)
                          (l/everyg #(fd/in % (fd/interval 12)) bass)
                          (l/everyg (fn [[r t f]]
                                      (l/conde
                                        [(triado (range 12) r t f)]))
                                    (map vector twinkle follower bass)))
        leader   (->> (phrase (repeat 1) twinkle)
                      (where :part (is :leader)))
        follower (->> (phrase (repeat 1) f)
                      (where :part (is :follower)))
        bass     (->> (phrase (repeat 1) b)
                      (where :part (is :bass)))]
    (->> leader
         (with follower)
         (with bass)
         (where :time speed)
         (where :duration speed)
         (where :pitch (comp C major))
         (play))
    all))

(defn aux-toneo "t2 is an auxillary tone"
  [t1 t2 t3] ; auxillary tone, also called neighbouring tone
  (l/all (fd/== t1 t3)
         (l/conde
           [(fd/eq (= (- t1 t2) 1))]
           [(fd/eq (= (- t2 t1) 1))])))

(defn passing-toneo "t2 is a passing tone"
  [t1 t2 t3]
  (l/conde
    [(fd/eq (= (- t3 t2) 1)             ; (< t1 t3)
            (= (- t3 t1) 2))]
    [(fd/eq (= (- t1 t2) 1)             ; (> t1 t3)
            (= (- t1 t3) 2))]))

;;;; Tests
(def chordtest
  (let [speed (bpm 120)
        [[f b] :as all] (l/run 2 [q]
                          (fresh [c1 c2 c3 c4  ]
                            (l/== q [c1 c2 c3 c4])
                            (fd/in c1 c2 c3 c4 (fd/interval 12))
                            (fd/== c1 c4) ; the first and last chord should be the same
                            ; a note should either be in the chord, or a passing note
                            (l/everyg (fn [[r t f]]
                                        (l/conde
                                          [(triado (range 12) r t f)]))
                                      (map vector twinkle follower bass))))
        leader   (->> (phrase (repeat 1) twinkle)
                      (where :part (is :leader)))
        follower (->> (phrase (repeat 1) f)
                      (where :part (is :follower)))
        bass     (->> (phrase (repeat 1) b)
                      (where :part (is :bass)))]
    (->> leader
         (with follower)
         (with bass)
         (where :time speed)
         (where :duration speed)
         (where :pitch (comp C major))
         (play))
    all))

(comment
  (print fandbs)
  (play-equal (first (second all-intervals-series)))
  (overtone/stop))

(comment
  from wikipedia
  :perfect V I in major, V i in minor
  :plagal IV I
  :interrupted V to any other, often V7 vi in major or V7 VI in minor
  :imperfect long explanation..)

(l/run 30 [q]
  (l/fresh [a b c]
    (triado (intervals->scale 1 [2 2 1 2 2 2 1])
            a b c)
    (l/== q [a b c])))

(l/run 30 [q]
  (l/fresh [a b c]
    (fd/in a b c domain)
;    (passing-toneo a b c)
    (aux-toneo a b c)
    (l/== q [a b c])))
