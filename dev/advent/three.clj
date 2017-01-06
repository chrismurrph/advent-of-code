(ns advent.three
  (:require [clojure.string :as s]
            [utils :as u]
            [clojure.java.io :as io]))

(defn possible? [sides]
  (let [ordered-sides (sort > sides)
        [biggest & others] ordered-sides
        sum-smaller (apply + others)]
    (> sum-smaller biggest)))

(defn trans [line]
  (let [three-num-str (s/split (s/trim line) #"\s+")
        three-num (mapv u/string->int three-num-str)]
    three-num))

;;
;; Just does 2nd part
;;
(defn x []
  (let [raw-series (line-seq (io/reader (io/resource "three.txt")))]
    (->> raw-series
         (mapv trans)
         (partition 3)
         (mapcat u/transpose)
         (filter possible?)
         count)))