(ns advent-2017.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

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

;;
;; Returns evenly divided or nil
;;
(defn -row->evenly-divided [row]
  (loop [xs row]
    (when (->> xs (drop 1) seq)
      (let [[top bottom] ((juxt last first) xs)
            ans (/ top bottom)]
        (if (int? ans)
          ans
          (recur (butlast xs)))))))

(defn row-checksum-2 [row]
  (loop [xs (sort row)]
    (let [ans (-row->evenly-divided xs)]
      (if ans
        ans
        (recur (next xs))))))

(defn x-2 []
  (let [input #_ex2 (get-input)]
    (->> input
         (map row-checksum-2)
         (reduce +))))

;; same as x-1 but factored out bit that will change for x-2
(defn x-1a []
  (let [input #_ex (get-input)]
    (->> input
         (map row-checksum-1)
         (reduce +))))

;; ans 43074
(defn x-1 []
  (let [input #_ex (get-input)]
    (->> input
         (map sort)
         (map (juxt (partial apply max) (partial apply min)))
         (map (partial apply -))
         (reduce +))))
