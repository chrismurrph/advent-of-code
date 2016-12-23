(ns advent.twenty-two
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [utils :as u])
  (:import (java.io StringReader BufferedReader)))

(defn all-but-last [in]
  (let [size (count in)]
    (apply str (take (dec size) in))))

(defn cell->obj [line-as-cells]
  (let [[one two three four five] line-as-cells]
    {:grid-id one
     :used    (->> three
                   (all-but-last)
                   (u/string->int))
     :avail   (->> four
                   (all-but-last)
                   (u/string->int))}))

(defn make-df-obj [line]
  (let [line-as-cells (string/split (string/trim line) #"\s+")
        ;_ (println cells)
        ]
    (cell->obj line-as-cells)))

;any two nodes (A,B), regardless of whether they are directly connected, such that:
;Node A is not empty (its Used is not zero).
;Nodes A and B are not the same node.
;The data on node A (its Used) would fit on node B (its Avail).
(defn spare-space-in-second [node-a node-b]
  (let [a-used (:used node-a)
        b-avail (:avail node-b)]
    (>= b-avail a-used)))

(defn viable-pairs [objects]
  (for [a objects
        b objects
        :let [space? (spare-space-in-second a b)]
        :when (and space? (not= a b) (-> a :used zero? not))
        ]
    [a b]))

(defn x []
  (let [
        raw-input (slurp "./advent/twenty_two.txt")
        ;raw-input steps
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        raw-df-lines (drop 2 in)
        objects (mapv make-df-obj raw-df-lines)
        pairs (viable-pairs objects)]
    (count pairs)
    ))