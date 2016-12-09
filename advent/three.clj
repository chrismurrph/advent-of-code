(ns advent.three
  (:require [clojure.string :as str]
            [utils :as u])
  (:import (java.io StringReader BufferedReader)))

(defn possible? [sides]
  (let [ordered-sides (sort > sides)
        [biggest & others] ordered-sides
        sum-smaller (apply + others)]
    (> sum-smaller biggest)))

(defn trans [line]
  (let [three-num-str (str/split (str/trim line) #"\s+")
        three-num (mapv u/string->int three-num-str)]
    three-num))

(defn x []
  (let [input (slurp "./advent/three.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        series (mapv trans raw-series)
        transformed-series (apply concat (map u/transpose (partition 3 series)))
        possibles (map possible? transformed-series)
        num-true (count (filter identity possibles))
        ]
    num-true
    ))