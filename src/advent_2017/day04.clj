(ns advent-2017.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def ex [["aa" "bb" "cc" "dd" "jj"]
         ["aa" "bb" "cc" "dd" "aa"]
         ["aa" "bb" "cc" "dd" "kk"]])

;; (def input (line-seq (io/reader (io/resource "day02"))))

(defn get-input []
  (->> (io/resource "2017/day04")
       slurp
       s/split-lines
       ;(take 2)
       dev/probe-off
       (mapv #(s/split % #" "))
       dev/probe-off
       ))

(defn has-dup? [ex]
  (->> (frequencies ex)
       vals
       (filter #(> % 1))
       seq))

(defn x-1 []
  (let [input (get-input)]
    (->> input
         (map #((complement has-dup?) %))
         (filter identity)
         count)))
