(ns advent.nineteen
  (:require [utils :as u]))

(defn make-eleves [num]
  (for [i (range num)]
    {:elf-num (inc i) :num-presents 1}))

(defn remove-elf [elves elf]
  (let [before-count (count elves)
        res (remove #{elf} elves)
        _ (assert (> before-count (count res)) (str "Did not remove " elf " from " elves))
        ]
    res))

(defn lupdate [list n function]
  (let [[head & tail] (drop n list)]
    (concat (take n list)
            (cons (function head) tail))))

(defn present-turns-slow [eleves]
  (loop [eleves eleves
         elf-num 1
         times 0]
    (let [eleves-count (count eleves)]
      (if (> eleves-count 1)
        (let [
              display? (= 0 (rem eleves-count 100000))
              _ (when display? (println "Going..."))
              ;_ (println "elves: " eleves)
              ;_ (println "elf-num: " elf-num)
              this-elf (some #(when (= (:elf-num %) elf-num) %) eleves)
              _ (assert this-elf (str "No elf found at <" elf-num "> from " eleves))
              next-elf (first (drop-while (fn [elf]
                                            (<= (:elf-num elf) elf-num)) eleves))
              next-elf (or next-elf (first eleves))
              _ (assert next-elf (str "No elf found after <" elf-num "> from " eleves))
              ]
          (if (zero? (:num-presents this-elf))
            (recur (remove-elf eleves this-elf) (:elf-num next-elf) (inc times))
            (let [current-elf-idx (u/index-of eleves this-elf)
                  _ (assert current-elf-idx (str "No idx found for <" this-elf "> from " (count eleves)))
                  next-elf-idx (u/index-of eleves next-elf)
                  _ (assert next-elf-idx (str "No idx found for <" next-elf "> from " (count eleves)))
                  ;_ (println "to update these:" current-elf-idx next-elf-idx "have" (count eleves))
                  num-presents-to-transfer (:num-presents (nth eleves next-elf-idx))
                  presents-exchanged-elves (-> eleves
                                               (lupdate current-elf-idx (fn [elf]
                                                                          (update elf :num-presents #(+ % num-presents-to-transfer))))
                                               (lupdate next-elf-idx (fn [elf]
                                                                       (update elf :num-presents #(- % num-presents-to-transfer)))))]
              (recur presents-exchanged-elves (:elf-num next-elf) (inc times))
              )))
        eleves))))

(defn present-turns [starting-eleves]
  (loop [left-vector []
         right-list starting-eleves]
    (let [[thief & [victim]] right-list]
      (if victim
        (let [num-presents-to-take (:num-presents victim)
              happy-thief (update thief :num-presents #(+ % num-presents-to-take))
              ]
          (recur (conj left-vector happy-thief) (drop 2 right-list)))
        (let [[one two] left-vector]
          (if two
            (if (nil? thief)
              (recur [] (seq left-vector))
              (recur [] (cons thief (seq left-vector))))
            (if (empty? right-list)
              one
              (recur [] (concat right-list left-vector)))))))))

(def num-eleves 3017957)
(def test-num-eleves 8)

(defn x []
  (let [elves (make-eleves num-eleves)]
    (present-turns elves)))

;; Thinking:
;; Consider five elves
;; one takes two, three takes four, five takes one
;; three takes five
;; 1 2 3 4 5 :step 1
;; 3 5 :step 2
;; 3 :step ?

;; Consider eight elves
;; one takes two, three takes four, five takes six, seven takes eight
;; one takes three, five takes seven
;; 1 2 3 4 5 6 7 8 :step 1
;; 1 3 5 7 :step 2
;; 1 5 :step 4

;;
;; BH beyond here
;;

;; the idea here is we can represent the data structure as a range
;; with a beginning and an end and a step
;; So each elf keeps its identity. When there are none before begin can move up
;; Similarly when none after end can move down.
;; When begin and end are equal there are none left.
;;
;; step is how many jump to get to the next one. At first is 1 and elf1 steals from immediate
;; neighbour elf2.
(defn next-level [{:keys [begin end step] :as st}]
  (println begin end step)
  (let [nstep (* 2 step)]
    (if (= 1 (mod (- (inc end) begin) nstep))
      (-> st
          (assoc :step nstep)
          (update-in [:begin] #(+ % nstep)))
      (-> st
          (assoc :step nstep)
          (update-in [:end] #(- % step))))))

(defn x-1 []
  (first
    (filter #(= (:begin %) (:end %))
            (iterate next-level {:begin 1 :end test-num-eleves :step 1}))))

;;
;; Part 2, try to solve in similar way
;;
;; Thinking
;; state a list of idx count v, initially [0 5 [1 2 3 4 5]]
;;
;; [1 4 [1 2 4 5]]
;;
;;
;; [2 3 [1 2 4]]
;; [3 2 [2 4]]
;; [4 1 [2]]
;;
;; 1 2 3 4 5
;; 1 2 4 5
;; 1 2 4
;; 2 4
;; 2

;;
;; returns [idx-after-which-missing-starts count-of-missing]
;;
(defn get-grab [coll]
  (assert (list? coll))
  (let [[idx [before after]] (first (drop-while (fn [[idx [prev nxt]]]
                                                  (or (nil? prev) (= (inc prev) nxt))) (map-indexed vector (map vector (cons nil coll) coll))))
        ]
    [(dec idx) (dec (- after before))]))

(defn canonical->list [{:keys [low high missing-idx missing-idx-len] :as st}]
  (let [_ (assert low)
        _ (assert high)
        big-list (range low (inc high))
        ;_ (println "big" big-list)
        ]
    (if missing-idx
      (seq (concat (take (inc missing-idx) big-list) (drop (inc (+ missing-idx missing-idx-len)) big-list)))
      big-list)))

;;
;; low and high are first and last, so '(3 7) has low of 3 and high of 7
;;
(defn canonical
  ([low high missing-idx missing-idx-len]
   {:low low :high high :missing-idx missing-idx :missing-idx-len missing-idx-len})
  ([low high]
   {:low low :high high}))

#_(defn list->canonical [in]
  (let [[missing-idx missing-idx-len] (get-grab in)]
    (canonical (first in) (last in) missing-idx missing-idx-len)))

(defn next-state-1 [{:keys [counted curr-idx in] :as st}]
  (let [
        grab (rem (+ curr-idx (if (odd? counted) (/ (dec counted) 2) (/ counted 2))) counted)
        _ (println "ITER" st "will grab" grab)
        ]
    {:curr-idx (inc curr-idx) :counted (dec counted) :in (concat (take grab in) (drop (inc grab) in))}))

(defn apply-grab [counted grab {:keys [low high missing-idx missing-idx-len] :as in}]
  (println "low, high: " low high)
  (println grab missing-idx counted)
  (let [res (cond
              (= (dec counted) grab) (if (= 2 counted)
                                       (canonical low low)
                                       (canonical low (dec high) missing-idx missing-idx-len))
              (zero? grab) (canonical (inc low) high (when missing-idx (dec missing-idx)) missing-idx-len)
              (nil? missing-idx) (canonical low high (dec grab) 1)
              (= grab (inc missing-idx)) (canonical low high missing-idx (inc missing-idx-len))
              (= grab missing-idx) (canonical low high (dec missing-idx) (inc missing-idx-len))
              :default (assert false (str "can't apply grab: " grab ", " in ", also " (canonical->list in) ", counted: " counted)))]
    ;(assert false (str "return apply grab: " grab ", " in ", " res ", at " counted))
    res
    ))

(defn next-state-2 [{:keys [counted curr-idx in] :as st}]
  (let [
        grab (rem (+ curr-idx (if (odd? counted) (/ (dec counted) 2) (/ counted 2))) counted)
        ;_ (println "ITER" st "will grab" grab)
        ]
    {:curr-idx (inc curr-idx) :counted (dec counted) :in (apply-grab counted grab in)}))

(defn x-2 []
  (let [curr-idx 3
        counted 2
        in '(2 4)
        st {:curr-idx curr-idx :counted counted :in in}]
    (next-state-2 st)))

(defn x-7 []
  (let [num-eleves 9
        curr-idx 0
        counted num-eleves
        in (range 1 (inc num-eleves))
        ;in (canonical 1 num-eleves)
        st {:curr-idx curr-idx :counted counted :in in}
        ;_ (println "init:" st)
        ]
    (first
      (filter #(= (:counted %) 1)
              (iterate next-state-1 st)))))

(defn x-3 []
  (let [num-eleves 9
        curr-idx 0
        counted num-eleves
        ;in (range 1 (inc num-eleves))
        in (canonical 1 num-eleves)
        st {:curr-idx curr-idx :counted counted :in in}
        ;_ (println "init:" st)
        ]
    (canonical->list
      (:in (first
             (filter #(= (:counted %) 1)
                     (iterate next-state-2 st)))))))

(defn x-4 []
  (let [got-1 (canonical->list (canonical 1 8 3 2))
        expected-1 '(1 2 3 4 7 8)
        got-2 (canonical->list (canonical 2 7 1 3))
        expected-2 '(2 3 7)
        got-3 (canonical->list (canonical 2 7))
        expected-3 '(2 3 4 5 6 7)]
    (assert (= got-1 expected-1) (str got-1 ", " expected-1))
    (assert (= got-2 expected-2) (str got-2 ", " expected-2))
    (assert (= got-3 expected-3) (str got-3 ", " expected-3))))

(defn x-5 []
  (assert (= [2 1] (get-grab '(1 2 3 5))))
  (assert (= [0 3] (get-grab '(3 7)))))

(defn x-6 []
  (let [in (range 1 (inc num-eleves))]
    (assert (= in (canonical->list (canonical 1 num-eleves))))))

;;
;; Bruce way now
;; Interesting observation on his code is that there is no need to know where up to.
;; Enough to draw down on the progress seq depending on whether state is even or odd.
;;

;;
;; Ready to take from first in prog
;; Needs to be sorted-set so when `seq` on it the seq will be in correct order for
;; chomping on it.
;;
(defn make-start-st [s]
  (let [st   (into (sorted-set) s)
        op   (int (/ (count st) 2))
        prog (drop op s)]
    (println "st is size: " (count st))
    (println "opp num is at: " op ", so size of prog:" (count prog))
    [st prog]))

;;
;; After we have chomped thru the 2nd half we can chomp from the beginning of st
;;
(defn handle-empty [[st prog :as x]]
  (if (empty? prog)
    (do
      ;(println "empty count" (count st))
      [st (seq st)])
    x))

;;
;; Choosing from the front of progress gives us a quick way to always be getting the opposite
;;
(defn drop-st* [[st progress :as x]]
  (if (= 1 (count st))
    x
    [(disj st (first progress)) (drop 1 progress)]))

(defn keep-1* [[st prog]] [st (drop 1 prog)])

;;
;; when completed doesn't do the f
;;
#_(defn skip-completed-dropping [f]
  (fn [x] (if (= 1 (count (first x))) x (f x))))

(def drop-st (comp drop-st* handle-empty))

(def keep-1 (comp keep-1* handle-empty))

#_(declare transition)

;;
;; With elves numbered 1->5 first count will be 5, so odd
;; progress exists just for picking the next opposite to steal from
;; Hence we remove 3 from the state, but 4 from the progress b/c next opposite is to be 5
;; Anyway on recursive call to transition* st will now be even, and only left, 5, will be removed,
;; then b/c empty st is put into prog and promptly has first removed.
;; Next transition* 4 will be removed then only one left
;;
(defn transition* [[st progress :as x]]
  ;(println "transition* with" x)
  (if (odd? (count st))
    (transition* (keep-1 (drop-st x)))
    (keep-1 (drop-st (drop-st x)))))

#_(def transition (skip-completed transition*))

(defn find-answer2 [s]
  (->> (iterate transition* (make-start-st s))
       (filter #(= 1 (count (first %))))
       first))

(def bruce-input 3004953)
(def my-input 3017957)
(def test-input 5)

(defn x-8 []
  (time (find-answer2 (range 1 (inc my-input)))))