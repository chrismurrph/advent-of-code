(ns advent-2017.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def ex [[5 1 9 5]
         [7 5 3]
         [2 4 6 8]])

(def ex2 [[5 9 2 8]
          [9 4 7 3]
          [3 8 6 5]])

(defn to-ints [xs]
  (mapv #(Integer/parseInt %) xs))

(defn get-input []
  (->> (io/resource "2017/day02")
       slurp
       s/split-lines
       (mapv (comp to-ints #(re-seq #"\d+" %)))))

(defn row-checksum-1 [xs]
  (->> xs
       sort
       ((juxt (partial apply max) (partial apply min)))
       (apply -)
       ))

(defn row->evenly-divided [xs]
  (->> (combo/combinations xs 2)
       (some (fn [xy]
               (let [res (apply / (sort-by - xy))]
                 (when (int? res)
                   res))))))

;; ans 280
(defn x-2 []
  (let [input #_ex2 (get-input)]
    (->> input
         (map row->evenly-divided)
         dev/probe-on
         (reduce +))))

;; ans 43074
(defn x-1 []
  (let [input #_ex (get-input)]
    (->> input
         (map row-checksum-1)
         (reduce +))))
