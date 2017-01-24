(ns advent-2015.day01
  (:require [clojure.java.io :as io]))

(defn part-1 []
  (let [input (slurp (io/resource "day01"))]
    (->> input
         (reductions (fn [acc ele] (({\( inc \) dec} ele) acc)) 0)
         last)))

(defn part-2 []
  (let [input (slurp (io/resource "day01"))]
    (->> input
         (reductions (fn [acc ele] (({\( inc \) dec} ele) acc)) 0)
         (take-while #(not= % -1))
         count)))
