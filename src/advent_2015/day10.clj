(ns advent-2015.day10
  (:require [utils :as u]))

(def input (u/digits 3113322113))

(defn expand-input [in]
  (->> in
       (partition-by identity)
       (map (juxt count first))
       flatten))

(defn x-1 []
  (->> (iterate expand-input input)
       (drop 50)
       first
       count))