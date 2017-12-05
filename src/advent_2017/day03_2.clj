(ns advent-2017.day03-2
  (:require [advent-2017.day03-1 :as day03-1]
            [clojure.test :refer :all]))

;;
;; I'm being lazy using an atom here. x-2 below seems made to use an iterator, as already has
;; `drop-while` and `first`. State would be made of of n, pos and what's inside this atom,
;; which is 'cells visited' with calculated value inside.
;; Actually we don't need pos because n gives us that (via ord->position, which knows the
;; memory path). Also using n you can get the calculated value that `drop-while` needs.
;;
(def default {[0 0] 1})
(def -pos->value (atom default))

(defn get-value [pos]
  (get @-pos->value pos))

(defn set-value [pos value]
  (swap! -pos->value assoc pos value))

(defn ord->position [memory-path]
  (fn [ord]
    (let [at-ord (nth memory-path ord)]
      (:pos at-ord))))

(def outliers [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])

(defn surrounding-cells [pos]
  (reduce
    (fn [acc ele]
      (conj acc (mapv + pos ele)))
    []
    outliers))

(defn calc-position-value! [pos]
  (let [existing-val (get-value pos)]
    (if existing-val
      existing-val
      (let [new-val (->> (surrounding-cells pos)
                         (map get-value)
                         (filter some?)
                         (reduce + 0))]
        (set-value pos new-val)
        new-val))))

;; ans: 330785
(defn x-2 []
  (reset! -pos->value default)
  (let [memory-path (iterate day03-1/iteree day03-1/start-state)
        ord->position (ord->position memory-path)]
    (->> (range)
         (map inc)
         (map ord->position)
         (map calc-position-value!)
         (drop-while #(<= % 325489))
         first)))

;; TESTS

(deftest get-and-set
  (reset! -pos->value default)
  (set-value [2 2] -30)
  (is (= -30
         (get-value [2 2]))))

(deftest as-per-text
  (reset! -pos->value default)
  (calc-position-value! [1 0])
  (calc-position-value! [1 1])
  (is (= 4
         (calc-position-value! [0 1]))))
