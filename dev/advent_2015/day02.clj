(ns advent-2015.day02
  (:require [clojure.java.io :as io]
            [utils :as u]))

(defn surface-area [l w h]
  (+ (* 2 l w) (* 2 w h) (* 2 h l)))

(defn area-smallest-side [l w h]
  (let [sorted (sort [l w h])]
    (reduce * (take 2 sorted))))

(defn perimeter-smallest-side [l w h]
  (let [sorted (sort [l w h])
        [length width] (take 2 sorted)]
    (* 2 (+ length width))))

(defn volume [l w h]
  (* l w h))

(defn ribbon-required [l w h]
  (+ (perimeter-smallest-side l w h) (volume l w h)))

(defn paper-required [l w h]
  (+ (surface-area l w h) (area-smallest-side l w h)))

(def input (line-seq (io/reader (io/resource "day02"))))

(defn parse-input [x]
  (->> (re-find #"(\d+)x(\d+)x(\d+)" x)
       (drop 1)
       (map u/string->int)))

(defn part-1 []
  (->> input
       (map parse-input)
       (map #(apply paper-required %))
       ;(take 4)
       (reduce +)))

(defn x-1 []
  (ribbon-required 1 1 10))

(defn part-2 []
  (->> input
       (map parse-input)
       (map #(apply ribbon-required %))
       ;(take 4)
       (reduce +)))
