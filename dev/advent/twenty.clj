(ns advent.twenty
  (:require [clojure.string :as str]
            [utils :as u])
  (:import (java.io StringReader BufferedReader)))

(defn make-obj [input]
  (let [[lower upper] (str/split input #"-")]
    {:lower (u/string->int lower) :upper (u/string->int upper)}))

;; would be better if did the diff to -1 being okay
(defn lower-abutting [int1 int2]
  (let [diff (- int2 int1)
        ;_ (println "lower diff" diff)
        ]
    (>= diff -1)))

;; would be better if did the diff to -1 being okay
(defn upper-abutting [int1 int2]
  (let [diff (- int1 int2)
        ;_ (println "upper diff" diff)
        ]
    (>= diff -1)))

(defn intersecting-ranges? [range1 range2]
  (assert (= (keys range1) [:lower :upper]) range1)
  (assert (= (keys range2) [:lower :upper]) range2)
  (when
    (or (and (lower-abutting (:lower range1) (:lower range2)) (upper-abutting (:upper range1) (:lower range2)))
        (and (lower-abutting (:lower range1) (:upper range2)) (upper-abutting (:upper range1) (:upper range2))))
    {:lower (min (:lower range1) (:lower range2))
     :upper (max (:upper range1) (:upper range2))}))

(defn merge-into-rest [ips]
  (let [[head & tail] ips]
    (reduce
      (fn [{:keys [res left-overs]} ele]
        (let [new-res (intersecting-ranges? res ele)]
          (if new-res
            {:res new-res :left-overs left-overs}
            {:res res :left-overs (conj left-overs ele)})))
      {:res head :left-overs []}
      tail)))

(defn x []
  (let [input (slurp "./advent/twenty.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        series (map make-obj raw-series)
        sorted (sort-by :lower series)
        examples (take 20 sorted)
        ;_ (println examples)
        ;in-tuples (partition 2 examples)
        ;_ (println in-tuples)
        ;res (map #(apply intersecting-ranges? %) in-tuples)
        res (merge-into-rest examples)
        ]
    res))
