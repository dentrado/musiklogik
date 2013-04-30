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

;; Utils
(def zip (partial map vector))

(defn unzip
  "takes a seq of pairs and returns (a vector of) two vectors (not lazy)."
  [pairs]
  (reduce (fn [[xs ys] [x y]]
            [(conj xs x) (conj ys y)])
          [[] []]
          pairs))

(defn transpose [matrix]
  (apply map vector matrix))

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

(defn passing-noteo
  [note]
  (fresh [p1 p2 p3]
    (fd/in p1 p2 p3 domain)
    (featurec note {:prev {:pitch p1}, :pitch p2, :next {:pitch p3}})
    (conde
      [(secondo p1 p2) (secondo p2 p3)]    ; (< prev next)
      [(secondo p3 p2) (secondo p2 p1)]))) ; (> prev next)

;; Chord contraints
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
(def horch   (phrase
              [1 1 1 1, 1 1 2, 1 1 2, 1 1 2]
              [0 1 2 3, 4 5 4, 3 1 6, 4 2 7])) ; `Horch was kommt von draussen rein' same as in strasheela example
(def twinkle (phrase
               [1 1 1 1, 1 1 2, 1 1 1 1, 1 1 2]
               [0 0 4 4, 5 5 4, 3 3 2 2, 1 1 0]))

(def melody (add-neighbours horch))
(def mel-bars (bars melody))

(def find-chords-example
  (let [chords (lvars 4)]
    (run 10 [q]
      (== q chords)
      (== (first chords) (last chords))
      (everyg triado chords)
      (everyg accompanyo (zip mel-bars chords)))))

(def find-melody-example
  (let [chords [[0 2 4] [3 5 7] [4 6 8] [3 5 7]
                [0 2 4] [4 6 8] [3 5 7] [0 2 4]]
        lmelody (lvars (+ 4 8 3 3, 4 8 3 3))
        melody (add-neighbours
                (phrase
                 (concat [1 1 1 1] (repeat 8 1/2) [1 1 2] [1 1 2]
                         [1 1 1 1] (repeat 8 1/2) [1 1 2] [1 1 2])
                 lmelody))
        mel-bars (bars melody)]
    (run 10 [c m]
      (== c chords) (== m melody)
      (== (first lmelody) 0)                ; start with the tonic
      (== (last lmelody) 0)                 ; end with the tonic
      (everyg #(fd/in % domain) lmelody)    ; constrain the pitches
      (everyg #(fd/distinct (map :pitch %)) ; make the melody more interesting by
              mel-bars)                     ; making notes within a bar distinct
      (everyg accompanyo (zip mel-bars chords)) ; harmonize melody with chords
      )))

;; Playback fns
(defn add-triads [triads melody]
  (let [[i iii v] (map #(->> (phrase (repeat 4) %)
                              (where :part (is :bass)))
                       (transpose triads))]
    (->> melody (with i) (with iii) (with v))))

(defn play-example [[chords melody] & {:keys [bpms scale]
                                       :or {bpms 120, scale (comp C major)}}]
  (let [speed (bpm bpms)]
    (->> melody
         (where :part (is :leader))
         (add-triads chords)
         (where :time speed)
         (where :duration speed)
         (where :pitch scale)
         (play))))

(play-example (first find-melody-example3))
