(ns advent-2019.day1
  (:require
    [clojure.java.io :as io]))

(defn to-int [s]
  (Long/parseLong s))

(defn fuel-a
  "Calculates the mass of fuel needed to move a mass (of anything)"
  [mass]
  (-> (/ mass 3)
      int
      (- 2)))

(defn fuel-b [mass]
  (loop [mass mass total 0]
    (let [fuel (fuel-a mass)]
      (if (< fuel 0)
        total
        (recur fuel (+ fuel total))))))

(defn x-1a []
  (fuel-a 12))

(defn x-2a []
  (fuel-a 14))

(defn x-3a []
  (fuel-a 1969))

(defn x-4a []
  (fuel-a 100756))

(defn solve []
  (let [reader (io/reader (io/resource "day1.edn"))
        lines (map to-int (line-seq reader))]
    (reduce
      (fn [acc mass]
        (+ acc (fuel-b mass)))
      0
      lines)))

(comment
  (solve))

(defn x-2b []
  (fuel-b 14))

(defn x-3b []
  (fuel-b 1969))

(defn x-4b []
  (fuel-b 100756))

