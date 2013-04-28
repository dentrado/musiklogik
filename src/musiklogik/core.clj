(ns musiklogik.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is]]
        [leipzig.melody]
        [leipzig.scale]
        [leipzig.live]
        [overtone.inst.sampled-piano :only [sampled-piano]])
  (:require [clojure.core.logic.fd :as fd]
            [overtone.live :as overtone]
            [overtone.synth.stringed :as strings]))

;; Leipzig example copypasta
(strings/gen-stringed-synth ektara 1 true)

(defn pick [distort amp {midi :pitch, start :time, length :duration}]
  (let [synth-id (overtone/at start
                              (ektara midi :distort distort :amp amp :gate 1))]
    (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

(defn piano [{pitch :pitch, start :time, length :duration}]
  (let [synth-id (overtone/at start (sampled-piano pitch))]
    (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

(comment
  (defmethod play-note :leader [note]
    (pick 0.7 1.0 note))
  (defmethod play-note :follower [note]
    (pick 0.3 1.0 note))
  (defmethod play-note :bass [note]
    (pick 0.9 0.2 (update-in note [:pitch] #(- % 12)))))

(defmethod play-note :leader [note]
  (piano note))
(defmethod play-note :follower [note]
  (piano note))
(defmethod play-note :bass [note]
  (piano  (update-in note [:pitch] #(- % 12))))

(defn play-equal [pitches]
  (let [speed (bpm 120)]
    (->> (phrase (repeat 1) pitches)
         (where :part (is :leader))
         (where :time speed)
         (where :duration speed)
         (where :pitch (comp C major))
         (play))))

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

(def domain (fd/interval 14)) ; Could be smaller?

(def horch   (phrase
              [1 1 1 1, 1 1 2, 1 1 2, 1 1 2]
              [0 1 2 3, 4 5 4, 3 1 6, 4 2 7])) ; `Horch was kommt von draussen rein' from strasheela example
(def twinkle (phrase
               [1 1 1 1, 1 1 2, 1 1 1 1, 1 1 2]
               [0 0 4 4, 5 5 4, 3 3 2 2, 1 1 0]))

;; Preprocessing functions
(defn bars
  "Returns the phrase divided up in bars. Each bar is 4 in duration."
  [phrase]
  (loop [[{dur :duration :as note} & rest] phrase
         count 0
         bar []
         bars []]
    (cond
     (not note)  (conj bars bar)
     (= count 4) (recur rest dur           [note]          (conj bars bar))
     :else       (recur rest (+ count dur) (conj bar note) bars))))

(defn add-neighbours
  "adds :prev and :next keys to all notes in the phrase,
   containing the previous and next notes, or nil if there is none."
  [phrase]
  (cons
   (assoc (first phrase) :prev nil :next (second phrase))
   (for [[a b c] (partition 3 1 [nil] phrase)]
     (assoc b :prev a :next c))))

;; Pitch constraints
(defn modo [a b remainder]
  (fresh [x y]
    (fd/in a b remainder x y domain)
    (fd/eq (= a (+ (* b x) remainder))
           (< remainder b))))

(defn normalizeo "normalize a pitch to be between 0-7"
  [p p-normalized]
  (fd/in p domain)
  (fd/in p-normalized (fd/interval 0 7))
  (conde
    [(fd/< p 7) (== p p-normalized)]
    [(fd/>= p 7) (fd/- p 7 p-normalized)]))

(defn intervalo [num pitch1 pitch2]
  (fresh [p1norm p2norm]
    (fd/in num pitch1 pitch2 p1norm p2norm domain)
    ;(fd/- pitch2 pitch1 num)
    (normalizeo pitch1 p1norm)
    (normalizeo pitch2 p2norm)
    ;(fd/- p2norm p1norm num)
    (conde
        [(fd/- p2norm p1norm num)]
        [(fd/eq (= (- (+ 7 p2norm) p1norm) num))] ; octave wraparound
        )))

(def unisono (partial intervalo 0))
(def secondo (partial intervalo 1))
(def thirdo  (partial intervalo 2))
(def fourtho (partial intervalo 3))
(def fiftho  (partial intervalo 4))

;; Note constraints
(defn aux-noteo
  [note] ; auxillary note, also called neighbouring note
  (fresh [p1 p2]
    (fd/in p1 p2 domain)
    (featurec note {:prev {:pitch p2}, :pitch p1, :next {:pitch p2}})
    (conde
      [(secondo p2 p1)]    ; (< prev next)
      [(secondo p1 p2)]))) ; (> prev next)

(defn passing-noteo "t2 is a passing tone"
  [note]
  (fresh [p1 p2 p3]
    (fd/in p1 p2 p3 domain)
    (featurec note {:prev {:pitch p1}, :pitch p2, :next {:pitch p3}})
    (conde
      [(secondo p1 p2) (secondo p2 p3)]    ; (< prev next)
      [(secondo p3 p2) (secondo p2 p1)]))) ; (> prev next)

;; chord contraints
(defne triado [chord]
  ([[a b c]] (thirdo a b) (fiftho a c)))

(defn in-chordeo [tone chord]
  (fresh [p p-norm]
    (featurec tone {:pitch p})
    (fd/in p p-norm domain) ;p-norm
    (normalizeo p p-norm)
    (membero p-norm chord)))

(defn legal-noteo
  [note chord]
  (conde
    [(in-chordeo note chord)]
    [(fresh [prev next]
       (featurec note {:prev prev :next next})
       (in-chordeo prev chord)
       (in-chordeo next chord)
       (conde [(passing-noteo note)] [(aux-noteo note)]))]))

(defn accompanyo [[bar chord]]
  (everyg #(legal-noteo % chord) bar))

;;;;;;;;;;;;;
(def melody (add-neighbours horch))
(def mel-bars (bars melody))

(comment
  (let [chords (lvars 4)
        [chords] (run 1 [q]
          (== q chords)
          (== (first chords) (last chords))
          (everyg triado          ; #(conde [(triado %)] [(== % [4 6 0 2])])
                  chords)
          (everyg accompanyo (map vector mel-bars chords)))]
    chords)

    (let [chords (lvars 4)
          chords (run 10 [q]
                   (== q chords)
                   (== (first chords) (last chords))
                   (everyg triado          ; #(conde [(triado %)] [(== % [4 6 0 2])])
                           chords)
                   (everyg find-chordeo (map vector mel-bars chords)))]
       chords)

)




(defn transpose [matrix]
  (apply map vector matrix))


(comment (let []
           (->> (phrase (repeat 1) pitches)
                (where :part (is :leader))
                (where :time speed)
                (where :duration speed)
                (where :pitch (comp C major))
                (play))))


(def chordprogressions '(([0 2 4] [2 4 6] [6 1 3] [0 2 4]) ;0
                        ([0 2 4] [0 2 4] [6 1 3] [0 2 4])  ;1
                        ([0 2 4] [1 3 5] [6 1 3] [0 2 4])  ;2
                        ([0 2 4] [2 4 13] [6 1 3] [0 2 4]) ;3
                        ([0 2 4] [3 5 14] [6 1 3] [0 2 4]) ;4
                        ([0 2 4] [4 6 1] [6 1 3] [0 2 4])  ;5
                        ([0 2 4] [3 5 0] [6 1 3] [0 2 4])  ;6
                        ([0 2 4] [4 6 8] [6 1 3] [0 2 4])  ;7
                        ([0 2 4] [3 5 7] [6 1 3] [0 2 4])  ;8
                        ([0 2 4] [0 9 4] [6 1 3] [0 2 4])));9


(let [speed (bpm 120)
      [as bs cs] (map #(phrase (repeat 4) %)
                      (transpose (nth chordprogressions 0)))
      i   (->> as (where :part (is :bass)))
      iii (->> bs (where :part (is :bass)))
      v  (->> cs (where :part (is :bass)))]
  (->> melody
       (where :part (is :leader))
       (with i)
       (with iii)
       (with v)
       (where :time speed)
       (where :duration speed)
       (where :pitch (comp C major))
       (play)))

(comment
    (run 5 [t c]
      (== c [0 2 4])
      (passing-noteo t)
      (fresh [a b c]
        (== t {:pitch 5 :prev {:pitch a} :next {:pitch b}})))


  )
