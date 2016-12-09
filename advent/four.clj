(ns advent.four
  (:require [clojure.string :as str]
            [utils :as u])
  (:import (java.io BufferedReader StringReader)))

(def input-1 "aaaaa-bbb-z-y-x-123[abxyz]")
(def input-2 "qzmt-zixmtkozy-ivhz-343[abxyz]")
(def input-3 "a-b-c-d-e-f-g-h-987[abcde]")

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def alphabet-size (count alphabet))
(def lookup-alphabet (into {} (map-indexed (fn [idx letter] [letter (inc idx)]) alphabet)))

(defn rotate [rotate-by]
  (fn [letter]
    (let [
          on-by (rem rotate-by alphabet-size)
          letter-at (get lookup-alphabet letter)]
      (if letter-at
        (let [
              ;_ (println "letter-at: " letter-at ", on-by: " on-by " for " letter)
              moved-on (+ on-by letter-at)
              ;_ (println "moved-on: " moved-on)
              in-range (rem moved-on alphabet-size)
              ;_ (println "in-range: " in-range)
              new-letter (nth alphabet (if (zero? in-range) (dec alphabet-size) (dec in-range)))]
          new-letter)
        \space))))

(defn decrypt [in]
  (let [
        ;_ (println "decrypt" in)
        before-sector-idx (str/last-index-of in "-")
        letters (take before-sector-idx in)
        sector-id (u/string->int (apply str (take-while #(not= \[ %) (drop (inc before-sector-idx) in))))
        rotate-f (rotate sector-id)
        rotated (map rotate-f letters)]
    {:decrypted (apply str rotated) :sector-id sector-id}))

(defn cf [cmp-a cmp-b]
  (let [size-diff (- (:size cmp-a) (:size cmp-b))
        ;_ (println "size-diff:" size-diff cmp-a cmp-b)
        ]
    (if (not (zero? size-diff))
      (- size-diff)
      (let [x-letter (:letter cmp-a)
            y-letter (:letter cmp-b)
            cf-res (compare x-letter y-letter)
            ;_ (println cf-res "from" x-letter y-letter)
            ]
        cf-res))))

(defn get-sector-id [in]
  (let [before-sector-idx (str/last-index-of in "-")
        sector-id (u/string->int (apply str (take-while #(not= \[ %) (drop (inc before-sector-idx) in))))
        letters (remove #{\-} (take before-sector-idx in))
        grouped (group-by identity letters)
        xs (map (fn [v] {:size (count v) :letter (first v)}) (vals grouped))
        sorted-xs (sort cf xs)
        ;_ (println sorted-xs)
        res (apply str (take 5 (map :letter sorted-xs)))
        chksum (u/between "[" "]" in)
        ;_ (println sector-id)
        ]
    (when (= res chksum)
      sector-id)))

(defn x-first-part []
  (let [input (slurp "./advent/four.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        series (remove nil? (map get-sector-id raw-series))
        ;_ (get-sector-id (nth raw-series 2))
        ]
    (apply + series)
    ))

(defn x []
  (let [input (slurp "./advent/four.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        series (map decrypt raw-series)
        possibilities (filter #(str/index-of (:decrypted %) "north") series)
        ]
    possibilities))

(defn x-2 []
  (decrypt input-2))

(defn x-3 []
  ((rotate 3) \y))
