(ns advent-2015.day05
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn cf [x y]
  ;(println x y (= x y))
  (= x y))

(defn enough-vowels-and-doubles [word]
  (loop [curr word
         vowles []
         twicely? false]
    (if (and twicely? (>= (count vowles) 3))
      true
      (if (nil? curr)
        false
        (let [[x & tail] curr
              y (first tail)
              new-twicely? (or twicely? (cf x y))]
          (if (#{\a \e \i \o \u} x)
            (recur tail (conj vowles x) new-twicely?)
            (recur tail vowles new-twicely?)))))))

(def input (line-seq (io/reader (io/resource "day05"))))

(defn part-1-nice-string [s]
  (assert (string? s))
  (if (or (s/index-of s "ab") (s/index-of s "cd") (s/index-of s "pq") (s/index-of s "xy"))
    false
    (enough-vowels-and-doubles s)))

(defn x-1 []
  (map part-1-nice-string ["ugknbfddgicrmopn" "aaa" "jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb"]))

(defn x-2 []
  (part-1-nice-string "aaa"))

(defn part-1 []
  (->> input
       (map part-1-nice-string)
       (filter identity)
       count
       ))
