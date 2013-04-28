(ns musiklogik.garbage)

;(def major [2 2 1 2 2 2 1])
;;Note
[dur pitch]

(defn semitoneo [a b] (fd/- b a 1))

(defn toneo [a b] (fd/- b a 2))

(defn multipleo "a is a multiple of b"
  [a b]
  (fresh [x] (fd/* b x a)))

(defn bars
  "Returns the phrase divided up in bars. Each bar is 4 in duration."
  [phrase]
  (loop [[[dur pitch :as tone] & rest] phrase
         count 0
         bar []
         bars []]
    (cond
     (not tone)  (conj bars bar)
     (= count 4) (recur rest dur           [tone]          (conj bars bar))
     :else       (recur rest (+ count dur) (conj bar tone) bars))))

;;; Triplets
;; the melody is padded with one extra note before and after to get the triplets right.

(defn triplet-bars
  "Returns the phrase divided up in bars. Each bar is 4 in duration."
  [triplet-phrase]
  (loop [[[_ [dur pitch :as tone] _ :as triplet] & rest] triplet-phrase
         count 0
         bar []
         bars []]
    (cond
     (not tone)  (conj bars bar)
     (= count 4) (recur rest dur           [triplet]          (conj bars bar))
     :else       (recur rest (+ count dur) (conj bar triplet) bars))))

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

(defn triado [chord]
  (l/fresh [a b c]
    (fd/in a b c domain)
    (toneo a b) ;; tone and semitone is not the right name when counting intervals *in the scale* right?
    (toneo b c)
    (l/== chord [a b c])))

(l/defne in-chordeo [tone chord]
  ([t [t _ _]])
  ([t [_ t _]])
  ([t [_ _ t]]))

(l/defne legal-toneo
  "checks that the middle tone in the triplet is legal"
  [triplet chord]
  ([[_ t2 _] c] (in-chordeo t2 c))
  ([[t1 t2 t3] c]
     (in-chordeo t1 c)
     (in-chordeo t3 c)
     (l/conde [(passing-toneo t1 t2 t3)] [(aux-toneo t1 t2 t3)])))

(def fandbs
  (let [melody horch
        speed (bpm 120)
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
                                    (map vector melody follower bass)))
        leader   (->> (phrase (repeat 1) melody)
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

;; old

(defn intervalo [num note1 note2]
  (l/fresh [p1 p2]
    (fd/in p1 p2 domain)
    (l/featurec note1 {:pitch p1})
    (l/featurec note2 {:pitch p2})
    (fd/- p2 p1 num)))

(def unisono (partial intervalo 0))
(def secondo (partial intervalo 1))
(def thirdo  (partial intervalo 2))
(def fourtho (partial intervalo 3))
(def fiftho  (partial intervalo 4))

(defn aux-noteo
  [note] ; auxillary note, also called neighbouring note
  (fresh [prev next]
    (featurec note {:prev prev :next next})
    (== prev next) ; prev and next are maps, we can't use fd/==.
    (conde
      [(secondo prev note)]    ; (< prev next)
      [(secondo note prev)])))

(defn passing-noteo "t2 is a passing tone"
  [note]
  (fresh [prev next]
   (featurec note {:prev prev :next next})
   (conde
     [(secondo prev note) (secondo note next)]    ; (< prev next)
     [(secondo next note) (secondo note prev)]))) ; (> prev next)

;;;; Tests
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

(comment
  (l/run 30 [q]
    (l/fresh [a b c]
      (triado (intervals->scale 1 [2 2 1 2 2 2 1])
              a b c)
      (l/== q [a b c]))))
