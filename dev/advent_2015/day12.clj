(ns advent-2015.day12
  (:require [clojure.java.io :as io]
            [utils :as u]
            [clojure.string :as s]))

(def input (slurp (io/resource "day12")))

(defn ->whole-number [ns]
  (assert (coll? ns))
  (u/string->int (apply str ns)))

(defn num-or-minus? [x]
  (boolean (or (= \- x)
               (u/char->int-not-strict x))))

(defn part-1 []
  (->> input
       (partition-by num-or-minus?)
       ;(drop 8)
       ;(take 10)
       (filter #(-> % first num-or-minus?))
       (map ->whole-number)
       u/probe-off
       (reduce +)
       ))

(defn ch-positions-of [ch coll]
  (u/positions #(= ch %) coll))

(defn x-1 []
  (let [start-positions (->> (ch-positions-of \{ input)
                             (map #(vector % :start)))
        red-positions (->> (u/indexes-of input ":\"red\"")
                           (map #(vector % :red)))
        end-positions (->> (ch-positions-of \} input)
                           (map #(vector % :end)))
        all (->> (concat start-positions red-positions end-positions)
                 (sort-by first))]
    all))
