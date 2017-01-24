(ns advent-2015.day05
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn enough-vowels-and-doubles [word]
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
        (if (#{\a \e \i \o \u} x)
          (recur tail (conj vowles x) new-twicely?)
          (recur tail vowles new-twicely?))))))

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

(defn x-3 []
  (map nice-part-2 ["qjhvhtzxzqqjkmpb" "xxyxx" "uurcxstgmygtbstg" "ieodomkazucvgmuy"]))

(def input (line-seq (io/reader (io/resource "day05"))))

(defn bad-pair [s]
  (or (s/index-of s "ab") (s/index-of s "cd") (s/index-of s "pq") (s/index-of s "xy")))

(defn part-1-nice-string [s]
  (assert (string? s))
  (when (not (bad-pair s))
    (enough-vowels-and-doubles s)))

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
       (map nice-part-2)
       (filter identity)
       count))
