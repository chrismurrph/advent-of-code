(ns advent-2019.day4
  (:require
    [clojure.java.io :as io]
    [au.com.seasoft.general.dev :as dev]))

(def example-num 111122)

(defn to-int [s]
  (Long/parseLong s))

(defn digit-ize [n]
  (->> n
       str
       (map (comp to-int str))))

(defn x-1 []
  (digit-ize example-num))

(defn not-decreasing? [partitioned]
  (->> partitioned
       (drop-while (fn [[a b]]
                     (>= b a)))
       empty?))

(defn contiguous-exists-a? [partitioned]
  (some (fn [[a b]]
          (= a b)) partitioned))

;;
;; Finding the first double used to be enough. Now we need to find all the doubles then exclude
;; any that are the same and next to each other.
;; To do this partition by same value. Remove any that are of length > 1. Any left means we have a
;; winner.
;;
(defn contiguous-exists-b? [partitioned]
  (->> partitioned
       (partition-by (fn [[a b]]
                       (when (= a b) b)))
       (remove #(> (count %) 1))
       (map first)
       (remove (fn [[a b]] (not= a b)))
       seq))

(defn partition-f [n]
  (partition 2 1 (digit-ize n)))

(defn x-5 []
  (contiguous-exists-b? (partition-f example-num)))

(defn x-4 []
  (partition-f example-num))

(defn num-satisfies-a? [n]
  (let [partitioned (partition-f n)
        double? (contiguous-exists-a? partitioned)]
    (when double?
      (not-decreasing? partitioned))))

(defn num-satisfies-b? [n]
  (let [partitioned (partition-f n)
        repeated-pos (contiguous-exists-b? partitioned)]
    (when repeated-pos
      (not-decreasing? partitioned))))

(defn x-2 []
  (contiguous-exists-b? (partition-f example-num)))

(defn generate-input []
  (let [begin 109165
        end 576723]
    (range begin (inc end))))

(defn solve []
  (->> (generate-input)
       (filter num-satisfies-b?)
       count))

(comment
  (solve))

