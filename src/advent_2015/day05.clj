(ns advent-2015.day05
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [utils :as u]))

(def vowels #{\a \e \i \o \u})

;; I bet Bruce didn't do it this way. So gonna do my own way more interesting than this
(defn enough-vowels-and-doubles-1? [word]
  (loop [curr word
         vowles []
         twicely? false]
    (cond
      (and twicely? (>= (count vowles) 3)) true
      (nil? curr) false
      :default
      (let [[x & tail] curr
            y (first tail)
            new-twicely? (or twicely? (= x y))]
        (if (vowels x)
          (recur tail (conj vowles x) new-twicely?)
          (recur tail vowles new-twicely?))))))

;; xazegov
(defn three-vowels? [word]
  (->> (filter #(vowels %) word)
       (drop 2)
       first
       ))

;; abcdde
(defn any-letter-appears-twice-conseq? [word]
  (->> (partition 2 1 word)
       (some (fn [[x y]] (= x y)))))

(defn y-1 []
  (three-vowels? "xazegov"))

(defn y-2 []
  (any-letter-appears-twice-conseq? "abcfde"))

;;
;; It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy)
;; or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
;; It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe),
;; or even aaa.
;;
(defn nice-part-2 [word]
  (loop [curr word
         pairs []
         pair-satisfied? false
         repeated-stradling? false]
    (cond
      (and pair-satisfied? repeated-stradling?) true
      (nil? curr) false
      :default
      (let [[x & tail] curr
            [y z] tail
            curr-pair (str y z)
            new-pair-satisfied? (or pair-satisfied? ((set pairs) curr-pair))
            new-repeated-stradling? (or repeated-stradling? (= x z))
            ]
        (recur tail (conj pairs (str x y)) new-pair-satisfied? new-repeated-stradling?)))))

(defn non-consecutive-pairs [word]
  (some
    (fn [[x & xs]] (some #(> (- % x) 1) xs))
    (vals
      (dev/probe-on (apply merge-with concat
                         (dev/probe-on (map-indexed (fn [a b] {b [a]}) (partition 2 1 word))))))))

;;
;; Given a bunch of numnbers in order from lowest -> highest, is there one that isn't right next to the first
;; some way is quicker b/c it short circuits the diff b/ween x and first that satisfies
;; max is fine too, just has to go through them all.
;;
(defn non-consecutive [[x & xs]]
  #_(> (- (apply max xs) x) 1)
  (some #(> (- % x) 1) xs)
  )

(defn non-consec-pairs [word]
  (->> (partition 2 word)
       (group-by identity)
       (some #(>= (-> % second count) 2))))

;; TOO complicated - see above
;;
;; merge-with always works with multiple maps
;; we want to recognise the same key coming more than once
;; normally merge will just choose the last
;; merge-with allows this overwriting to be something else - so lets use concat for two vectors
;; vals being greater than 1 would be the answer, except you'd get consequtive
(defn my-non-consecutive-pairs [word]
  (->> (apply merge-with concat (map-indexed (fn [a b] {b [a]}) (partition 2 1 word)))
       vals
       (some #(and (> (count %) 1) (non-consecutive %)))
       ))

;;
;; Testing myself not resorting to recursion
;;
(defn nice-part-2-again [word]
  (and
    (my-non-consecutive-pairs word)
    (some (fn [[x _ z]] (= x z)) (partition 3 1 word))))

(defn x-3 []
  (map nice-part-2-again (take 1 ["qjhvhtzxzqqjkmpb" "xxyxx" "uurcxstgmygtbstg" "ieodomkazucvgmuy"])))

(defn x-4 []
  (my-non-consecutive-pairs "aaaa" #_"qjhqqj"))

(defn x-5 []
  (non-consec-pairs "qjhqqj"))

(def input (line-seq (io/reader (io/resource "day05"))))

(defn bad-pair [s]
  (or (s/index-of s "ab") (s/index-of s "cd") (s/index-of s "pq") (s/index-of s "xy")))

(defn part-1-nice-string [s]
  (assert (string? s))
  (when (not (bad-pair s))
    (and (three-vowels? s) (any-letter-appears-twice-conseq? s))
    #_(enough-vowels-and-doubles-1? s)))

(defn x-1 []
  (map part-1-nice-string ["ugknbfddgicrmopn" "aaa" "jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb"]))

(defn x-2 []
  (part-1-nice-string "aaa"))

(defn part-1 []
  (->> input
       (map part-1-nice-string)
       (filter identity)
       count))

(defn part-2 []
  (->> input
       (map nice-part-2-again)
       (filter identity)
       count))
