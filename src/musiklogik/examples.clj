(ns musiklogik.examples
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is]]
        [musiklogik.core]
        [leipzig.melody]
        [leipzig.scale]
        [leipzig.live])
  (:require [overtone.live :as overtone]
            [clojure.core.logic.fd :as fd]))

(def twinkle (phrase
               [1 1 1 1, 1 1 2, 1 1 1 1, 1 1 2]
               [0 0 4 4, 5 5 4, 3 3 2 2, 1 1 0]))

(def horch (phrase ; `Horch was kommt von draussen rein' same as in strasheela example
              [1 1 1 1, 1 1 2, 1 1 2, 1 1 2]
              [0 1 2 3, 4 5 4, 3 1 6, 4 2 7]))

(def melody (add-neighbours horch))
(def mel-bars (bars melody))

(def find-chords-example
  (let [chords (lvars 4)]
    (run 4 [c m]
      (== c chords)
      (== m melody)
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
    (run 1 [c m]
      (== c chords) (== m melody)
      (== (first lmelody) 0)                ; start with the tonic
      (== (last lmelody) 0)                 ; end with the tonic
      (everyg #(fd/in % domain) lmelody)    ; constrain the pitches
      (everyg #(fd/distinct (map :pitch %)) ; make the melody more interesting by
              mel-bars)                     ; making notes within a bar distinct
      (everyg accompanyo (zip mel-bars chords)) ; harmonize melody with chords
      )))


(comment

  (run 10 [pitch chord]
    (triado chord)
    (in-chordeo {:pitch pitch} chord))

  (run 10 [p1 p2 p3]
    (passing-noteo {:prev {:pitch p1}
                    :pitch p2
                    :next {:pitch p3}}))

  (play-melody melody)

  (play-example (first find-chords-example))

  (play-example (first find-chords-example)
                :scale (comp F minor))

  (play-chords [[0 2 4] [3 5 7] [4 6 8] [3 5 7] [0 2 4] [4 6 8] [3 5 7] [0 2 4]]
               :bpms 200)

  ;(play-example (first find-melody-example))

  (play-example precomputed)

  (play-example precomputed
                :bpms 200
                :scale (comp A minor))

  )

(def precomputed
  '[[[0 2 4] [3 5 7] [4 6 8] [3 5 7] [0 2 4] [4 6 8] [3 5 7] [0 2 4]]
   ({:next {:pitch 7, :duration 1, :time 1}, :prev nil, :pitch 0, :duration 1, :time 0} {:next {:pitch 2, :duration 1, :time 2}, :prev {:pitch 0, :duration 1, :time 0}, :pitch 7, :duration 1, :time 1} {:next {:pitch 9, :duration 1, :time 3}, :prev {:pitch 7, :duration 1, :time 1}, :pitch 2, :duration 1, :time 2} {:next {:pitch 3, :duration 1/2, :time 4}, :prev {:pitch 2, :duration 1, :time 2}, :pitch 9, :duration 1, :time 3} {:next {:pitch 10, :duration 1/2, :time 9/2}, :prev {:pitch 9, :duration 1, :time 3}, :pitch 3, :duration 1/2, :time 4} {:next {:pitch 5, :duration 1/2, :time 5N}, :prev {:pitch 3, :duration 1/2, :time 4}, :pitch 10, :duration 1/2, :time 9/2} {:next {:pitch 6, :duration 1/2, :time 11/2}, :prev {:pitch 10, :duration 1/2, :time 9/2}, :pitch 5, :duration 1/2, :time 5N} {:next {:pitch 14, :duration 1/2, :time 6N}, :prev {:pitch 5, :duration 1/2, :time 5N}, :pitch 6, :duration 1/2, :time 11/2} {:next {:pitch 13, :duration 1/2, :time 13/2}, :prev {:pitch 6, :duration 1/2, :time 11/2}, :pitch 14, :duration 1/2, :time 6N} {:next {:pitch 12, :duration 1/2, :time 7N}, :prev {:pitch 14, :duration 1/2, :time 6N}, :pitch 13, :duration 1/2, :time 13/2} {:next {:pitch 4, :duration 1/2, :time 15/2}, :prev {:pitch 13, :duration 1/2, :time 13/2}, :pitch 12, :duration 1/2, :time 7N} {:next {:pitch 3, :duration 1, :time 8N}, :prev {:pitch 12, :duration 1/2, :time 7N}, :pitch 4, :duration 1/2, :time 15/2} {:next {:pitch 4, :duration 1, :time 9N}, :prev {:pitch 4, :duration 1/2, :time 15/2}, :pitch 3, :duration 1, :time 8N} {:next {:pitch 11, :duration 2, :time 10N}, :prev {:pitch 3, :duration 1, :time 8N}, :pitch 4, :duration 1, :time 9N} {:next {:pitch 3, :duration 1, :time 12N}, :prev {:pitch 4, :duration 1, :time 9N}, :pitch 11, :duration 2, :time 10N} {:next {:pitch 10, :duration 1, :time 13N}, :prev {:pitch 11, :duration 2, :time 10N}, :pitch 3, :duration 1, :time 12N} {:next {:pitch 5, :duration 2, :time 14N}, :prev {:pitch 3, :duration 1, :time 12N}, :pitch 10, :duration 1, :time 13N} {:next {:pitch 0, :duration 1, :time 16N}, :prev {:pitch 10, :duration 1, :time 13N}, :pitch 5, :duration 2, :time 14N} {:next {:pitch 7, :duration 1, :time 17N}, :prev {:pitch 5, :duration 2, :time 14N}, :pitch 0, :duration 1, :time 16N} {:next {:pitch 2, :duration 1, :time 18N}, :prev {:pitch 0, :duration 1, :time 16N}, :pitch 7, :duration 1, :time 17N} {:next {:pitch 4, :duration 1, :time 19N}, :prev {:pitch 7, :duration 1, :time 17N}, :pitch 2, :duration 1, :time 18N} {:next {:pitch 3, :duration 1/2, :time 20N}, :prev {:pitch 2, :duration 1, :time 18N}, :pitch 4, :duration 1, :time 19N} {:next {:pitch 4, :duration 1/2, :time 41/2}, :prev {:pitch 4, :duration 1, :time 19N}, :pitch 3, :duration 1/2, :time 20N} {:next {:pitch 6, :duration 1/2, :time 21N}, :prev {:pitch 3, :duration 1/2, :time 20N}, :pitch 4, :duration 1/2, :time 41/2} {:next {:pitch 5, :duration 1/2, :time 43/2}, :prev {:pitch 4, :duration 1/2, :time 41/2}, :pitch 6, :duration 1/2, :time 21N} {:next {:pitch 11, :duration 1/2, :time 22N}, :prev {:pitch 6, :duration 1/2, :time 21N}, :pitch 5, :duration 1/2, :time 43/2} {:next {:pitch 12, :duration 1/2, :time 45/2}, :prev {:pitch 5, :duration 1/2, :time 43/2}, :pitch 11, :duration 1/2, :time 22N} {:next {:pitch 13, :duration 1/2, :time 23N}, :prev {:pitch 11, :duration 1/2, :time 22N}, :pitch 12, :duration 1/2, :time 45/2} {:next {:pitch 14, :duration 1/2, :time 47/2}, :prev {:pitch 12, :duration 1/2, :time 45/2}, :pitch 13, :duration 1/2, :time 23N} {:next {:pitch 13, :duration 1, :time 24N}, :prev {:pitch 13, :duration 1/2, :time 23N}, :pitch 14, :duration 1/2, :time 47/2} {:next {:pitch 5, :duration 1, :time 25N}, :prev {:pitch 14, :duration 1/2, :time 47/2}, :pitch 13, :duration 1, :time 24N} {:next {:pitch 3, :duration 2, :time 26N}, :prev {:pitch 13, :duration 1, :time 24N}, :pitch 5, :duration 1, :time 25N} {:next {:pitch 7, :duration 1, :time 28N}, :prev {:pitch 5, :duration 1, :time 25N}, :pitch 3, :duration 2, :time 26N} {:next {:pitch 2, :duration 1, :time 29N}, :prev {:pitch 3, :duration 2, :time 26N}, :pitch 7, :duration 1, :time 28N} {:next {:pitch 0, :duration 2, :time 30N}, :prev {:pitch 7, :duration 1, :time 28N}, :pitch 2, :duration 1, :time 29N} {:next nil, :prev {:pitch 2, :duration 1, :time 29N}, :pitch 0, :duration 2, :time 30N})])


(comment
  ;; some chords generated by find-chords-example
  (([0 2 4] [2 4 6] [6 1 3] [0 2 4])
   ([0 2 4] [0 2 4] [6 1 3] [0 2 4])
   ([0 2 4] [1 3 5] [6 1 3] [0 2 4])
   ([0 2 4] [2 4 13] [6 1 3] [0 2 4])))
