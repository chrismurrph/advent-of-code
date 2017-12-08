(ns advent-2017.day02-transducer
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [utils :as u]))

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

(def xform-row->evenly-divided (comp
                                 (map (partial sort-by -))
                                 (map (partial apply /))
                                 (filter int?)))

(defn row->evenly-divided-2 [xs]
  (->> (u/combinations xs 2)
       (sequence xform-row->evenly-divided)
       first))

(defn row->evenly-divided-1 [xs]
  (->> (u/combinations xs 2)
       (map (fn [xy]
               (let [res (apply / (sort-by - xy))]
                 res)))
       (filter int?)
       first))

;; ans 280
(defn x-2 []
  (let [input (get-input)]
    (->> input
         (map row->evenly-divided-2)
         (reduce +))))