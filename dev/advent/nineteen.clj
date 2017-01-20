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
  (let [big-list (range low (inc high))
        _ (println "big" big-list)
        ]
    (seq (concat (take (inc missing-idx) big-list) (drop (inc (+ missing-idx missing-idx-len)) big-list)))))

;;
;; low and high are first and last, so '(3 7) has low of 3 and high of 7
;;
(defn canonical [low high missing-idx missing-idx-len]
  {:low low :high high :missing-idx missing-idx :missing-idx-len missing-idx-len})

#_(defn list->canonical [in]
  (let [[missing-idx missing-idx-len] (get-grab in)]
    (canonical (first in) (last in) missing-idx missing-idx-len)))

(defn next-state [{:keys [counted curr-idx in] :as st}]
  (let [
        grab (rem (+ curr-idx (if (odd? counted) (/ (dec counted) 2) (/ counted 2))) counted)
        _ (println "ITER" st "will grab" grab)
        ]
    {:curr-idx (inc curr-idx) :counted (dec counted) :in (concat (take grab in) (drop (inc grab) in))}))

(defn x-2 []
  (let [curr-idx 3
        counted 2
        in '(2 4)
        st {:curr-idx curr-idx :counted counted :in in}]
    (next-state st)))

(defn x-3 []
  (let [num-eleves 8
        curr-idx 0
        counted num-eleves
        in (range 1 (inc num-eleves))
        st {:curr-idx curr-idx :counted counted :in in}
        ;_ (println "init:" st)
        ]
    (first
      (filter #(= (:counted %) 1)
              (iterate next-state st)))))

(defn x-4 []
  (let [got-1 (canonical->list (canonical 1 8 3 2))
        expected-1 '(1 2 3 4 7 8)
        got-2 (canonical->list (canonical 2 7 1 3))
        expected-2 '(2 3 7)]
    (assert (= got-1 expected-1) (str got-1 ", " expected-1))
    (assert (= got-2 expected-2) (str got-2 ", " expected-2))))

(defn x-5 []
  (assert (= [2 1] (get-grab '(1 2 3 5))))
  (assert (= [0 3] (get-grab '(3 7)))))