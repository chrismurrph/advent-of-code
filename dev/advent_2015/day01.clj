(ns advent-2015.day01
  (:require [clojure.java.io :as io]))

(defn part-1 []
  (let [input (slurp (io/resource "day01"))]
    (->> input
         (reductions (fn [acc ele] (({\( inc \) dec} ele) acc)) 0)
         last)))

;; Was overkill to use reductions and last
(defn part-1-again []
  (let [input (slurp (io/resource "day01"))]
    (->> input
         (reduce (fn [acc ele]
                   (({\( inc \) dec} ele) acc)) 0))))

;;
;; Here we want the count of the reductions.
;; Recursion would do it. map-indexed and reduce ending with (reduced) would do it too
;; reductions is lazy so gives you the chance to drop or take while
;; and see how many was needed to get there
;;
(defn part-2 []
  (let [input (slurp (io/resource "day01"))]
    (->> input
         (reductions (fn [acc ele] (({\( inc \) dec} ele) acc)) 0)
         (take-while #(not= % -1))
         count)))

(defn iter-fn [[paren n]]
  (({\( inc \) dec} paren) n))

;;
;; How is reductions different to iterating on state.
;; A: Need to process big line of parens.
;; iterate good for singular state, transformed
;; reductions is good for going over a list and each time getting back a singular,
;; where need to stop at some unknown point.
;; Here showing don't care about the answers along the way, or even final result, just
;; number of times it took to get there.
;;
(defn part-2-again []
  (let [input (slurp (io/resource "day01"))]
    #_(->> input
         (iterate iter-fn []))))
